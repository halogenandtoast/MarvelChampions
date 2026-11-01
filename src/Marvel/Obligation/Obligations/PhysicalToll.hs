module Marvel.Obligation.Obligations.PhysicalToll (
  physicalToll,
  PhysicalToll (..),
  physicalTollEffect,
  PhysicalTollEffect (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier
    ( HasModifiersFor, Modifier(AdditionalResourceCost) )
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ability.Types (HasAbilities)

import Marvel.Ref
import Marvel.Window qualified as W

physicalToll :: ObligationCard PhysicalToll
physicalToll = obligation (PhysicalToll . (`With` Meta False)) Cards.physicalToll

-- | Whether it stays in play until an event is played.
newtype Meta = Meta {lingering :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype PhysicalToll = PhysicalToll (Attrs Obligation `With` Meta)
  deriving anyclass (HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsObligation PhysicalToll where
  toObligationAttrs (PhysicalToll (attrs `With` _)) = attrs

-- | You may flip to alter-ego form. Then either exhaust Stephen Strange to remove it from
-- the game, or the next event you play costs 3 additional resources, this obligation
-- staying in play until you play one.
instance RunMessage PhysicalToll where
  runMessage msg o@(PhysicalToll (attrs `With` meta)) = runQueueT $ case msg of
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
        isUnexhaustedAlterEgo <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustStephenStrangeRemovePhysicalToll" (exhaust identityId >> removeFromGame attrs)
          | isUnexhaustedAlterEgo
          ]
            <> [ labeled (countLabel "nextEventCostsMore" 3) do
                  createEffect Cards.physicalToll attrs (IdentityEntity $ IdentityWithId identityId)
                  cardSpecific attrs "lingers" ()
               ]
        pure o
      -- staying in play, it is discarded by its effect once an event is played
      ResolvedObligation _ | lingering meta -> pure o
      _ -> PhysicalToll . (`With` meta) <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "lingers" _ -> pure . PhysicalToll $ attrs `With` Meta True
    _ -> PhysicalToll . (`With` meta) <$> runMessage msg attrs

-- | The next event the player plays costs 3 more; once it is played, this and the
-- obligation it comes from are discarded.
newtype PhysicalTollEffect = PhysicalTollEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

physicalTollEffect :: CardEffect PhysicalTollEffect
physicalTollEffect =
  effectWith PhysicalTollEffect Cards.physicalToll $
    modifiersL .~ [AdditionalResourceCost (CardWithType EventType) 3]

instance RunMessage PhysicalTollEffect where
  runMessage msg e@(PhysicalTollEffect attrs) = runQueueT $ case msg of
    CheckWindows windows
      | ident : _ <- [i | W.Window W.After (W.EventPlayed i _ _) <- windows] -> do
          played <- effectValidFor attrs (toRef ident)
          when played do
            disableEffect (effectId attrs)
            removeFromPlay (refocus (effectSource attrs) :: Target)
          pure e
    _ -> PhysicalTollEffect <$> runMessage msg attrs
