module Marvel.Obligation.Obligations.CareForCassie (
  careForCassie,
  careForCassieEffect,
  CareForCassie (..),
  CareForCassieEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Card.Code
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Window qualified as W

careForCassie :: ObligationCard CareForCassie
careForCassie = obligation CareForCassie Cards.careForCassie

newtype CareForCassie = CareForCassie (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust Scott Lang → remove Care for Cassie
-- from the game; or choose and discard 1 card from your hand, you cannot change form
-- until your next turn ends, and discard this obligation.
instance RunMessage CareForCassie where
  runMessage msg o@(CareForCassie attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        when isHero $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        pure o
      ResolveObligation identityId -> do
        canExhaust <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustScottLangRemoveCareForCassie" (exhaust identityId >> removeFromGame attrs)
          | canExhaust
          ]
            <> [ labeled "$label.discardCardCannotChangeForm" do
                  discardFromHand identityId 1
                  createEffect Cards.careForCassie attrs (IdentityEntity $ IdentityWithId identityId)
               ]
        pure o
      _ -> CareForCassie <$> runMessage msg attrs
    _ -> CareForCassie <$> runMessage msg attrs

-- | Whether a turn of the player's has begun since it was created: revealed during their
-- turn, it lasts through the next one.
newtype Meta = Meta {turnBegun :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype CareForCassieEffect = CareForCassieEffect (Attrs Effect `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEffect CareForCassieEffect where
  toEffectAttrs (CareForCassieEffect (a `With` _)) = a

-- | "You cannot change form until your next turn ends."
careForCassieEffect :: CardEffect CareForCassieEffect
careForCassieEffect =
  effectWith (CareForCassieEffect . (`With` Meta False)) Cards.careForCassie (modifiersL .~ [CannotChangeForm])

instance RunMessage CareForCassieEffect where
  runMessage msg (CareForCassieEffect (attrs `With` meta)) = runQueueT $ case msg of
    -- told by the window a turn's beginning opens
    CheckWindows windows | idents <- [i | W.Window W.After (W.TurnBegan i) <- windows], notNull idents -> do
      begun <- anyM (effectValidFor attrs . IdentityRef) idents
      CareForCassieEffect . (`With` Meta (turnBegun meta || begun)) <$> runMessage msg attrs
    IdentityEndedTurn ident | turnBegun meta -> do
      whenM (effectValidFor attrs (IdentityRef ident)) $ disableEffect (effectId attrs)
      CareForCassieEffect . (`With` meta) <$> runMessage msg attrs
    _ -> CareForCassieEffect . (`With` meta) <$> runMessage msg attrs
