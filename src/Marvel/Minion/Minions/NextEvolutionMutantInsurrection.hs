module Marvel.Minion.Minions.NextEvolutionMutantInsurrection where

import Marvel.Minion.Runner
import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability
    ( AbilityType(ForcedResponse, ForcedInterrupt),
      HasAbilities(..),
      windowAbility )
import Marvel.Card ()
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hand ()
import Marvel.Identity.Types ()
import Marvel.Keyword ()
import Marvel.Matchers hiding (UnexhaustedIdentity, UnexhaustedAlly)
import Marvel.Matchers qualified as Matchers ()
import Marvel.Modifier ()
import Marvel.NextEvolution.Modulars
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Resource.Types
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Entity ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types ()


import Marvel.Ref ()
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted ()

newtype MutantInsurrectionMinion = MutantInsurrectionMinion (Attrs Minion `With` (Natural,Natural))
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ne40185 :: MinionCard MutantInsurrectionMinion
ne40185 = minion (MutantInsurrectionMinion . (`With` ((0,0) :: (Natural,Natural)))) Cards.ne40185 (Sch 1) (Atk 1) (HP 3)

ne40186 :: MinionCard MutantInsurrectionMinion
ne40186 = minion (MutantInsurrectionMinion . (`With` ((0,0) :: (Natural,Natural)))) Cards.ne40186 (Sch 0) (Atk 4) (HP 6)

ne40187 :: MinionCard MutantInsurrectionMinion
ne40187 = minion (MutantInsurrectionMinion . (`With` ((0,0) :: (Natural,Natural)))) Cards.ne40187 (Sch 1) (Atk 2) (HP 4)

ne40188 :: MinionCard MutantInsurrectionMinion
ne40188 = minion (MutantInsurrectionMinion . (`With` ((0,0) :: (Natural,Natural)))) Cards.ne40188 (Sch 1) (Atk 2) (HP 5)


instance IsMinion MutantInsurrectionMinion where
  toMinionAttrs (MutantInsurrectionMinion (attrs `With` _)) = attrs
  overMinionAttrs f (MutantInsurrectionMinion (attrs `With` meta)) = MutantInsurrectionMinion (f attrs `With` meta)
  minionDisplayCounters (MutantInsurrectionMinion (_ `With` (_,counters))) = counters
instance IsRef MutantInsurrectionMinion where
  toRef = toRef . toMinionAttrs

instance HasAbilities MutantInsurrectionMinion where
  getAbilities a@(MutantInsurrectionMinion (attrs `With` (_,_))) =
    [windowAbility a 1 window kind NoCost (runAbility a 1)]
    where
      self = EnemyWithId $ EnemyMinionId $ minionId attrs
      (window,kind) = case toCardCode attrs of
        "40185" -> (W.EnemyActivates W.When self AnyIdentity, ForcedInterrupt)
        "40187" -> (W.EnemyAttacked W.When self AnyIdentity, ForcedInterrupt)
        _ -> (W.EnemyAttacked W.After self AnyIdentity, ForcedResponse)

instance HasModifiersFor MutantInsurrectionMinion where
  getModifiersFor _ target (MutantInsurrectionMinion (attrs `With` (bonus,_)))
    | target == MinionRef (minionId attrs), toCardCode attrs == "40185" = pure [AttackModifier bonus, SchemeModifier bonus]
  getModifiersFor _ _ _ = pure []

instance RunMessage MutantInsurrectionMinion where
  runMessage msg a@(MutantInsurrectionMinion (attrs `With` (bonus,counters))) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      case toCardCode attrs of
        "40185" -> do
          bonus <- printedResourcesInHand you Energy
          pure $ MutantInsurrectionMinion (attrs `With` (bonus,counters))
        "40186" -> do
          n <- printedResourcesInHand you Physical
          discardTopOfDeck you n
          pure a
        "40187" -> do
          n <- printedResourcesInHand you Mental
          when (n >= 2) $ giveStatus you StunnedStatus
          when (n >= 4) $ exhaust you
          pure a
        "40188" -> do
          placeCounters attrs 1
          cardSpecific attrs "samuraiChoice" (you, counters + 1)
          pure a
        _ -> pure a
    CardSpecific (isTarget attrs -> True) "samuraiChoice" (fromJSON -> Success (you,n :: Natural)) -> do
      discardable <- controlledCardsWithCostAtLeast you n
      immune <- cannotTakeDamageFrom (IdentityRef you) (toSource attrs)
      chooseOne you $
        [labeled "$label.neSamuraiDamage" (damageIdentity you attrs $ toDamage n FromAbility) | not immune]
        <> [labeled "$label.neSamuraiDiscard" (cardSpecific attrs "samuraiDiscard" (you,n)) | notNull discardable]
      pure a
    CardSpecific (isTarget attrs -> True) "samuraiDiscard" (fromJSON -> Success (you,n :: Natural)) -> do
      discardable <- controlledCardsWithCostAtLeast you n
      unless (null discardable) $ chooseOrRunOne you [targeting target (removeFromPlay target) | target <- discardable]
      pure a
    CheckWindows windows | any activationEnded windows -> do
      attrs' <- runMessage msg attrs
      pure $ MutantInsurrectionMinion (attrs' `With` (0,counters))
    PlaceCounters target n | target == toTarget attrs -> pure $ MutantInsurrectionMinion (attrs `With` (bonus,counters+n))
    RemoveCounters target n | target == toTarget attrs -> pure $ MutantInsurrectionMinion (attrs `With` (bonus,subtractNatural n counters))
    _ -> MutantInsurrectionMinion . (`With` (bonus,counters)) <$> runMessage msg attrs
   where
    activationEnded (W.Window W.After (W.EnemyActivation enemy _)) = enemy == EnemyMinionId (minionId attrs)
    activationEnded _ = False
