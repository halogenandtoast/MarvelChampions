module Marvel.Villain.Villains.AgeOfApocalypseFourHorsemen where

import Marvel.Prelude
import Marvel.Id
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Scenario.AgeOfApocalypseFourHorsemen
import Marvel.MainScheme.Cards qualified as MainSchemes
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaFourHorsemenVillain = AoaFourHorsemenVillain (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

warA :: VillainCard AoaFourHorsemenVillain
warA = villainWith AoaFourHorsemenVillain Cards.warA (Sch 1) (Atk 2) (HP $ PerPlayer 9) (stageL .~ 1)

warB :: VillainCard AoaFourHorsemenVillain
warB = villainWith AoaFourHorsemenVillain Cards.warB (Sch 2) (Atk 3) (HP $ PerPlayer 12) (stageL .~ 2)

famineA :: VillainCard AoaFourHorsemenVillain
famineA = villainWith AoaFourHorsemenVillain Cards.famineA (Sch 2) (Atk 1) (HP $ PerPlayer 9) (stageL .~ 1)

famineB :: VillainCard AoaFourHorsemenVillain
famineB = villainWith AoaFourHorsemenVillain Cards.famineB (Sch 3) (Atk 2) (HP $ PerPlayer 12) (stageL .~ 2)

pestilenceA :: VillainCard AoaFourHorsemenVillain
pestilenceA = villainWith AoaFourHorsemenVillain Cards.pestilenceA (Sch 2) (Atk 1) (HP $ PerPlayer 9) (stageL .~ 1)

pestilenceB :: VillainCard AoaFourHorsemenVillain
pestilenceB = villainWith AoaFourHorsemenVillain Cards.pestilenceB (Sch 3) (Atk 2) (HP $ PerPlayer 12) (stageL .~ 2)

deathA :: VillainCard AoaFourHorsemenVillain
deathA = villainWith AoaFourHorsemenVillain Cards.deathA (Sch 1) (Atk 2) (HP $ PerPlayer 9) (stageL .~ 1)

deathB :: VillainCard AoaFourHorsemenVillain
deathB = villainWith AoaFourHorsemenVillain Cards.deathB (Sch 2) (Atk 3) (HP $ PerPlayer 12) (stageL .~ 2)

instance HasModifiersFor AoaFourHorsemenVillain where
  getModifiersFor _ (VillainRef vid) (AoaFourHorsemenVillain attrs) | vid == villainId attrs = do
    others <- filter (/= vid) <$> selectList AnyVillain
    standing <- anyM (fmap (> 0) . horsemanHitPoints) others
    protected <- horsemanHasMinimumHitPoints vid
    pure [CannotBeDefeated | standing || protected]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaFourHorsemenVillain where
  getAbilities a@(AoaFourHorsemenVillain attrs) = [limitedWindowAbility a 1
    (W.EnemyAttacked W.After (EnemyWithId $ EnemyVillainId $ villainId attrs) AnyIdentity)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]
instance RunMessage AoaFourHorsemenVillain where
  runMessage msg a@(AoaFourHorsemenVillain attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      hp <- horsemanHitPoints (villainId attrs)
      when (hp > 0) $ for_ ([you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows]) $ horsemanResponse (villainId attrs)
      pure a
    CardSpecific (isTarget attrs -> True) "aoaHorsemanResponse" (fromJSON -> Success you) -> do
      case toBaseCardCode (toCardCode (getCardDef attrs)) of
        "45081" -> do
          upgrades <- selectList $ UpgradeControlledBy $ IdentityWithId you
          supports <- selectList $ SupportControlledBy $ IdentityWithId you
          chooseAndDiscardControlled you $ map UpgradeRef upgrades <> map SupportRef supports
        "45082" -> discardFromDeck you 10 attrs
        "45083" -> createEffect MainSchemes.theHorsemenOfApocalypse attrs $ IdentityEntity $ IdentityWithId you
        "45084" -> do
          characters <- selectList $ CharacterControlledBy $ IdentityWithId you
          damageCharacters attrs (toDamage 1 FromAbility) characters
        _ -> pure ()
      pure a
    _ -> AoaFourHorsemenVillain <$> runMessage msg attrs
