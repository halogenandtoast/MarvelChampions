module Marvel.Minion.Minions.NextEvolutionExtremeMeasures where

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

newtype ExtremeMeasuresMinion = ExtremeMeasuresMinion (Attrs Minion)
  deriving anyclass (IsMinion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40180 :: MinionCard ExtremeMeasuresMinion
ne40180 = minion ExtremeMeasuresMinion Cards.ne40180 (Sch 2) (Atk 2) (HP 4)

ne40181 :: MinionCard ExtremeMeasuresMinion
ne40181 = minion ExtremeMeasuresMinion Cards.ne40181 (Sch 1) (Atk 1) (HP 5)

ne40182 :: MinionCard ExtremeMeasuresMinion
ne40182 = minion ExtremeMeasuresMinion Cards.ne40182 (Sch 1) (Atk 1) (HP 3)

ne40183 :: MinionCard ExtremeMeasuresMinion
ne40183 = minion ExtremeMeasuresMinion Cards.ne40183 (Sch 0) (Atk 3) (HP 5)


instance HasAbilities ExtremeMeasuresMinion where
  getAbilities a@(ExtremeMeasuresMinion attrs)
    | toCardCode attrs == "40181" = [windowAbility a 1
        (W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity)
        ForcedResponse NoCost (runAbility a 1)]
    | otherwise = []

instance HasModifiersFor ExtremeMeasuresMinion where
  getModifiersFor _ target (ExtremeMeasuresMinion attrs) = case toCardCode attrs of
    "40181" -> pure [HandSizeModifier 1 | target == IdentityRef (minionEngagedIdentity attrs)]
    "40182" -> pure [AttackDamageReductionUnlessAttackerHasTrait Trait.Tiny 1 | target == MinionRef (minionId attrs)]
    _ -> pure []

instance RunMessage ExtremeMeasuresMinion where
  runMessage msg a@(ExtremeMeasuresMinion attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      case toCardCode attrs of
        "40180" -> do
          characters <- selectList (CharacterControlledBy $ IdentityWithId you)
          stunnable <- filterM canStunCharacter characters
          damageable <- filterM (fmap not . flip cannotTakeDamageFrom (toSource attrs) . characterRef) characters
          chooseOne you $
            [labeled "$label.neStunEachCharacter" (for_ characters (`giveStatus` StunnedStatus)) | notNull stunnable]
            <> [labeled "$label.neDamageEachCharacter" (damageCharacters attrs (toDamage 1 FromAbility) characters) | notNull damageable]
        "40182" -> do
          highest <- highestControlled you UpgradeType
          unless (null highest) $ chooseOrRunOne you [targeting target (returnToHand target) | target <- highest]
        "40183" -> do
          highest <- highestControlled you SupportType
          chooseOne you $
            [labeled "$label.neWildsideAttacks" (enemyAttacks mid you)]
            <> [labeled "$label.neReturnHighestSupport" (cardSpecific attrs "returnSupport" you) | notNull highest]
        _ -> pure ()
      pure a
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand you
      discardTopOfDeck you (2 * fromIntegral (length hand))
      pure a
    CardSpecific (isTarget attrs -> True) "returnSupport" (fromJSON -> Success you) -> do
      highest <- highestControlled you SupportType
      unless (null highest) $ chooseOrRunOne you [targeting target (returnToHand target) | target <- highest]
      pure a
    _ -> ExtremeMeasuresMinion <$> runMessage msg attrs
