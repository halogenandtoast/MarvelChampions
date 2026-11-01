module Marvel.Rules.NextEvolutionMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attack
import Marvel.Ally.Cards qualified as Allies
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Matchers
import Marvel.Query
import Marvel.Modifier
import Marvel.Trait (Trait (Possessed))
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "Next Evolution shared mechanics" do
  it "does not treat a direct attack against an ally as an attack against Spider-Man" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      ally <- putIntoPlay you "01083"
      clearHand you
      let aid = allyIdOf ally
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacksAlly you aid)]
      offers isAnyAbility `shouldReturn` False
      (length <$> handOf you) `shouldReturn` 0
      pass
      resolve []

  it "keeps a possessed ally and attached Malice as two separately targetable minions" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      ally <- putIntoPlay you "01083"
      malice <- encounterCard Minions.ne40199
      resolve [PutEncounterCardIntoPlay you malice]
      resolve [MinionMessage (minionOf malice) $ MinionDamaged (toSource you) $ toDamage 1 FromAbility]
      let host = MinionId $ unAllyId $ allyIdOf ally
      getsGame (HashMap.member host . gameMinions) `shouldReturn` True
      getsGame (HashMap.member (minionOf malice) . gameMinions) `shouldReturn` True
      selectAny (MinionWithId host <> MinionWithTrait Possessed) `shouldReturn` True
      selectAny (MinionWithId (minionOf malice) <> MinionEngagedWith AnyIdentity) `shouldReturn` False
      selectAny (MinionWithId host <> MinionEngagedWith (IdentityWithId you)) `shouldReturn` True
      mods <- getModifiers (MinionRef $ minionOf malice)
      liftIO do
        mods `shouldSatisfy` elem CannotActivate
        mods `shouldSatisfy` elem CannotBeDefeated
      minionDamageOf (minionOf malice) `shouldReturn` Just 1

  it "does not defeat attached Malice again after she heals and takes damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01083"
      malice <- encounterCard Minions.ne40199
      resolve [PutEncounterCardIntoPlay (player1 tg) malice]
      resolve [MinionMessage (minionOf malice) $ MinionDamaged GameRef $ toDamage 1 FromAbility]
      second <- putIntoPlay (player1 tg) "01084"
      resolve [MinionMessage (minionOf malice) (MinionHealed 1)]
      resolve [MinionMessage (minionOf malice) $ MinionDamaged GameRef $ toDamage 2 FromAbility]
      getsGame (HashMap.member (allyIdOf second) . gameAllies) `shouldReturn` True
      minionDamageOf (minionOf malice) `shouldReturn` Just 2

  it "discards Malice when her possessed host leaves play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01083"
      malice <- encounterCard Minions.ne40199
      resolve [PutEncounterCardIntoPlay (player1 tg) malice]
      resolve [MinionMessage (minionOf malice) $ MinionDamaged GameRef $ toDamage 1 FromAbility]
      let host = MinionId $ unAllyId $ allyIdOf ally
      resolve [RemoveFromPlay (MinionRef host)]
      getsGame (HashMap.member (minionOf malice) . gameMinions) `shouldReturn` False
      (map toCardCode <$> encounterDiscard) >>= (`shouldSatisfy` elem "40199")

  it "restores the possessed ally if Malice is removed independently" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01083"
      malice <- encounterCard Minions.ne40199
      resolve [PutEncounterCardIntoPlay (player1 tg) malice]
      resolve [MinionMessage (minionOf malice) $ MinionDamaged GameRef $ toDamage 1 FromAbility]
      resolve [RemoveFromPlay (MinionRef $ minionOf malice)]
      getsGame (HashMap.member (allyIdOf ally) . gameAllies) `shouldReturn` True
      getsGame (HashMap.member (MinionId $ unAllyId $ allyIdOf ally) . gameMinions) `shouldReturn` False

  it "protects Morlocks from removal abilities and costs but allows damage defeat into victory" do
    tg <- testGame ["spider-man"]
    inGame tg do
      morlock <- putIntoPlay (player1 tg) "40079"
      resolve [ReturnToHand (AllyRef $ allyIdOf morlock)]
      getsGame (HashMap.member (allyIdOf morlock) . gameAllies) `shouldReturn` True
      selectAny (AllyWithId (allyIdOf morlock) <> AllyThatCanLeavePlay) `shouldReturn` False
      resolve [AllyMessage (allyIdOf morlock) $ AllyDamaged GameRef $ toDamage 5 FromAbility]
      getsGame (HashMap.member (allyIdOf morlock) . gameAllies) `shouldReturn` False
      (map toCardCode <$> getVictoryDisplay) >>= (`shouldSatisfy` elem "40079")

  it "does not let Regroup replace a protected Morlock's defeat with a return to hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      regroup <- putIntoPlay you "19032"
      morlock <- putIntoPlay you "40079"
      vid <- villainIdNow
      runNow [AllyMessage (allyIdOf morlock) $ AllyDamaged (VillainRef vid) $ toDamage 5 FromAttack]
      offers (isAbilityOfCard regroup) `shouldReturn` False
      resolve []
      getsGame (HashMap.member (allyIdOf morlock) . gameAllies) `shouldReturn` False
      (map toCardCode <$> getVictoryDisplay) >>= (`shouldSatisfy` elem "40079")

  it "does not offer Regroup when an unprotected Victory ally is defeated" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      regroup <- putIntoPlay you "19032"
      jubilee <- putIntoPlay you "32088b"
      selectAny (AllyWithId (allyIdOf jubilee) <> AllyThatCanLeavePlay) `shouldReturn` True
      vid <- villainIdNow
      runNow [AllyMessage (allyIdOf jubilee) $ AllyDamaged (VillainRef vid) $ toDamage 10 FromAttack]
      offers (isAbilityOfCard regroup) `shouldReturn` False
      resolve []
      (map toCardCode <$> getVictoryDisplay) >>= (`shouldSatisfy` elem "32088b")
