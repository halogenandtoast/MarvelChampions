{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Obligation.Obligations.AgentsOfShield where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype AosObligation = AosObligation (Attrs Obligation `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass HasModifiersFor

instance IsObligation AosObligation where
  toObligationAttrs (AosObligation (a `With` _)) = a
  obligationDisplayCounters (AosObligation (_ `With` n)) = n

aos50134, aos50179 :: ObligationCard AosObligation
aos50134 = obligation (AosObligation . (`With` 4)) Cards.aos50134
aos50179 = obligation (AosObligation . (`With` 0)) Cards.aos50179

instance HasAbilities AosObligation where
  getAbilities (AosObligation (a `With` _)) = case obligationOwner a of
    Nothing -> []
    Just you -> case toCardCode a of
      "50134" -> [windowAbility a 1 matcher ForcedResponse NoCost (runAbility a 1)
        | matcher <- [W.IdentityAttacked W.After (IdentityWithId you) AnyEnemy, W.EnemyAttacked W.After AnyEnemy (IdentityWithId you)]]
      _ -> [ability a idx AlterEgoAction OwnsThis cost (runAbility a idx)
        | (idx,cost) <- [(2,ExhaustAnyIdentityCost),(3,ExhaustCardsWithTraitCost Trait.Shield 1 1)]]

instance RunMessage AosObligation where
  runMessage msg e@(AosObligation (a `With` n)) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> pure $ AosObligation (a {obligationOwner = Just you} `With` n)
    ObligationMessage oid ResolvedObligation{} | oid == obligationId a -> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      payable <- canAffordCost you (toSource a) (ResourceCost Nothing)
      difficulty <- getDifficulty
      chooseEffect you $
        [labeled "$label.aosSpendResource" (payThen you a (ResourceCost Nothing) $ pure ()) | payable]
        <> [labeled "$label.aosBystanderThreat" (placeThreatOnEach a (if difficulty == Expert then 2 else 1) MainScheme)]
      when (n <= 1) $ removeFromPlay a
      pure $ AosObligation (a `With` subtractNatural 1 n)
    RanAbility you (isTarget a -> True) idx _ _ | idx `elem` [2,3] -> do
      searchAndRevealEncounterCard you (CardWithType MinionType <> CardWithTrait Trait.Shield)
      removeFromPlay a
      pure e
    _ -> AosObligation . (`With` n) <$> runMessage msg a
