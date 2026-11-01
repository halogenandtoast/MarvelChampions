module Marvel.Obligation.Obligations.AgeOfApocalypseStandardIII where

import Marvel.Prelude
import Marvel.AgeOfApocalypse.StandardIII
import Marvel.Card
import Marvel.Entity
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Matchers
import Marvel.Cost (Cost(NoCost))
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Game.Source ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Window qualified as W

newtype StandardIIIObligation = StandardIIIObligation (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

drawingNear :: ObligationCard StandardIIIObligation
drawingNear = obligation StandardIIIObligation Cards.drawingNear

instance HasAbilities StandardIIIObligation where
  getAbilities (StandardIIIObligation a) = discardToDiscardAbility a HeroSpecificCard
    <> [windowAbility a 2 (W.TurnBegins $ IdentityWithId you) ForcedResponse NoCost (runAbility a 2)
       | Just you <- [obligationOwner a]]

instance RunMessage StandardIIIObligation where
  runMessage msg e@(StandardIIIObligation a) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ discardToDiscard a you HeroSpecificCard
    RanAbility you (isTarget a -> True) 2 _ _ -> do
      discardFromDeck you 1 a
      pure e
    WithDiscarded target FromDeck cards | isTarget a target -> do
      for_ (obligationOwner a) $ \you -> addPursuit you (fromIntegral $ length $ concatMap (printedResources . getCardDef) cards)
      pure e
    _ -> StandardIIIObligation <$> runMessage msg a
