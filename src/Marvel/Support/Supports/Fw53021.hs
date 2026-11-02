{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.Fw53021 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Card.PlayerCard (printedResourcesFor)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Support.Types

import Marvel.Identity.Types (Field(PlayerIdentityHand))
fw53021 :: SupportCard Fw53021
fw53021 = support Fw53021 Cards.fw53021

newtype Fw53021 = Fw53021 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53021 where
  getAbilities a@(Fw53021 attrs) =
    [ability a 1 Action (OwnsThis <> ExtendedCardExists (InHandOf You $ BasicCardMatches $ CardWithType ResourceType)) ExhaustCost (runAbility a 1) | null $ supportHeldCards attrs]
    <> [(ability a 2 Resource ForAnyPlayer NoCost (runAbility a 2)) {abilityChoices = [runAbility a 2, Pay $ SpentCardPayment card <> PrintedResourcesPayment (printedResources $ getCardDef card) <> ResourcePaymentFromPlayerCard card]} | card <- take 1 $ supportHeldCards attrs]

instance RunMessage Fw53021 where
  runMessage msg a@(Fw53021 attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "mgHoldCard" (fromJSON -> Success card) -> pure $ Fw53021 attrs {supportHeldCards = supportHeldCards attrs <> [card]}
    CardSpecific (isTarget attrs -> True) "mgRemoveHeld" (fromJSON -> Success cid) -> pure $ Fw53021 attrs {supportHeldCards = filter ((/= cid) . pcCardId) $ supportHeldCards attrs}
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      cards <- filter (cardMatch $ CardWithType ResourceType) <$> projectMap PlayerIdentityHand unHand you
      chooseOne you [targeting card $ placePlayerCardUnder attrs card | card <- cards]
      pure a
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      for_ (headMay $ supportHeldCards attrs) $ \card -> do
        cardSpecific attrs "mgRemoveHeld" (pcCardId card)
        discardCard card
      pure a
    _ -> Fw53021 <$> runMessage msg attrs
