module Marvel.Minion.Minions.NextEvolutionNastyBoys where

import Marvel.Minion.Runner
import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers hiding (UnexhaustedIdentity, UnexhaustedAlly)
import Marvel.Matchers qualified as Matchers
import Marvel.Modifier
import Marvel.NextEvolution.Modulars
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Entity
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NastyBoysMinion = NastyBoysMinion (Attrs Minion `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ne40112 :: MinionCard NastyBoysMinion
ne40112 = minion (NastyBoysMinion . (`With` (0 :: Natural))) Cards.ne40112 (Sch 1) (Atk 2) (HP 4)

ne40113 :: MinionCard NastyBoysMinion
ne40113 = minion (NastyBoysMinion . (`With` (0 :: Natural))) Cards.ne40113 (Sch 1) (Atk 2) (HP 3)

ne40114 :: MinionCard NastyBoysMinion
ne40114 = minion (NastyBoysMinion . (`With` (0 :: Natural))) Cards.ne40114 (Sch 1) (Atk 2) (HP 4)

ne40115 :: MinionCard NastyBoysMinion
ne40115 = minion (NastyBoysMinion . (`With` (0 :: Natural))) Cards.ne40115 (Sch 2) (Atk 1) (HP 3)

ne40116 :: MinionCard NastyBoysMinion
ne40116 = minion (NastyBoysMinion . (`With` (0 :: Natural))) Cards.ne40116 (Sch 1) (Atk 1) (HP 5)


instance IsMinion NastyBoysMinion where
  toMinionAttrs (NastyBoysMinion (attrs `With` _)) = attrs
  overMinionAttrs f (NastyBoysMinion (attrs `With` meta)) = NastyBoysMinion (f attrs `With` meta)
  minionDisplayCounters (NastyBoysMinion (_ `With` counters)) = counters
instance IsRef NastyBoysMinion where
  toRef = toRef . toMinionAttrs

instance HasAbilities NastyBoysMinion where
  getAbilities a@(NastyBoysMinion (attrs `With` counters)) = case toCardCode attrs of
    "40112" -> [windowAbility a 1 (W.EnemyAttacked W.When self AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]
    "40116" -> [windowAbility a 1 (W.EnemyAttackedAnyTarget W.When self) ForcedInterrupt NoCost (runAbility a 1)]
    _ -> []
    where self = EnemyWithId $ EnemyMinionId $ minionId attrs

instance HasModifiersFor NastyBoysMinion where
  getModifiersFor _ (MinionRef mid) (NastyBoysMinion (attrs `With` counters)) | mid == minionId attrs, toCardCode attrs == "40114" = pure [KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage NastyBoysMinion where
  runMessage msg a@(NastyBoysMinion (attrs `With` counters)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      case toCardCode attrs of
        "40112" -> do
          ready <- readyControlledCharacters you
          chooseOne you [targeting (characterRef character) (exhaustFriendlyCharacter character) | character <- ready]
        "40116" -> do
          placeCounters attrs 1
          changeAttack (minionId attrs) (fromIntegral $ counters + 1)
        _ -> pure ()
      pure a
    MinionMessage mid (RevealMinion you) | mid == minionId attrs, toCardCode attrs == "40115" -> do
      characters <- selectList (CharacterControlledBy $ IdentityWithId you)
      for_ characters (`giveStatus` StunnedStatus)
      pure a
    Boost (RevealedAsBoost target enemy) | target == toTarget attrs -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "40112" -> do
          ready <- readyControlledCharacters you
          chooseOne you [targeting (characterRef character) (exhaustFriendlyCharacter character) | character <- ready]
        "40113" -> do
          let card = MkEncounterCard (CardId $ unMinionId $ minionId attrs) (getCardDef attrs)
          afterMatching (\case CheckWindows windows -> any (\case W.Window W.After (W.EnemyActivation who _) -> who == enemy; _ -> False) windows; _ -> False)
            (shuffleIntoEncounterDeck [card])
        "40114" -> case enemy of
          EnemyVillainId _ -> whenM (enemyMatches AttackingEnemy enemy) $ attackGains enemy Piercing
          _ -> pure ()
        "40115" -> giveStatus you StunnedStatus
        _ -> pure ()
      pure a
    PlaceCounters target n | target == toTarget attrs -> pure $ NastyBoysMinion (attrs `With` (counters+n))
    RemoveCounters target n | target == toTarget attrs -> pure $ NastyBoysMinion (attrs `With` subtractNatural n counters)
    _ -> NastyBoysMinion . (`With` counters) <$> runMessage msg attrs
