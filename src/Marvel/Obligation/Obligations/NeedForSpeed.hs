module Marvel.Obligation.Obligations.NeedForSpeed (
  needForSpeed,
  needForSpeedEffect,
  NeedForSpeed (..),
  NeedForSpeedEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Entity ()
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ref
import Marvel.Window qualified as W

needForSpeed :: ObligationCard NeedForSpeed
needForSpeed = obligation NeedForSpeed Cards.needForSpeed

newtype NeedForSpeed = NeedForSpeed (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust Pietro Maximoff → remove Need for
-- Speed from the game; or exhaust your identity, you cannot ready it until your next
-- turn ends, and discard this obligation. Exhausting is no option for an exhausted
-- identity (RR Choose (Option)); with neither, it is only discarded.
instance RunMessage NeedForSpeed where
  runMessage msg o@(NeedForSpeed attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when isHero $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        NeedForSpeed <$> runMessage msg attrs
      ResolveObligation identityId -> do
        ready <- identityMatches UnexhaustedIdentity identityId
        alterEgo <- identityMatches AlterEgoIdentity identityId
        when ready $
          chooseOrRunOne identityId $
            [ labeled "$label.exhaustPietroMaximoffRemoveNeedForSpeed" (exhaust identityId >> removeFromGame attrs)
            | alterEgo
            ]
              <> [ labeled "$label.exhaustYourIdentityCannotReady" do
                    exhaust identityId
                    createEffect Cards.needForSpeed attrs (IdentityEntity $ IdentityWithId identityId)
                 ]
        pure o
      _ -> NeedForSpeed <$> runMessage msg attrs
    _ -> NeedForSpeed <$> runMessage msg attrs

-- | Whether a turn of the player's has begun since it was created: revealed during their
-- turn, it lasts through the next one.
newtype Meta = Meta {turnBegun :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NeedForSpeedEffect = NeedForSpeedEffect (Attrs Effect `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEffect NeedForSpeedEffect where
  toEffectAttrs (NeedForSpeedEffect (a `With` _)) = a

-- | "You cannot ready your identity until your next turn ends."
needForSpeedEffect :: CardEffect NeedForSpeedEffect
needForSpeedEffect =
  effectWith (NeedForSpeedEffect . (`With` Meta False)) Cards.needForSpeed (modifiersL .~ [CannotReady])

instance RunMessage NeedForSpeedEffect where
  runMessage msg (NeedForSpeedEffect (attrs `With` meta)) = runQueueT $ case msg of
    -- told by the window a turn's beginning opens
    CheckWindows windows | idents <- [i | W.Window W.After (W.TurnBegan i) <- windows], notNull idents -> do
      begun <- anyM (effectValidFor attrs . IdentityRef) idents
      NeedForSpeedEffect . (`With` Meta (turnBegun meta || begun)) <$> runMessage msg attrs
    IdentityEndedTurn ident | turnBegun meta -> do
      whenM (effectValidFor attrs (IdentityRef ident)) $ disableEffect (effectId attrs)
      NeedForSpeedEffect . (`With` meta) <$> runMessage msg attrs
    _ -> NeedForSpeedEffect . (`With` meta) <$> runMessage msg attrs
