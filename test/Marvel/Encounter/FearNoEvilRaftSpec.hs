module Marvel.Encounter.FearNoEvilRaftSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Minion.Types qualified as Minion
import TestImport

spec :: Spec
spec = describe "Fear No Evil Raft prisoners" do
  it "Absorbing Man keeps tucked cards and counts distinct resource types for each attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      mid <- addMinion you Cards.street60144
      tucked <- stackDeck you ["01090","01089"]
      _ <- setEncounterDeck (replicate 5 Cards.street60188) []
      resolve [MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 2
      resolve [MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 5
      held <- getZoneHeldCards (MinionRef mid)
      liftIO $ map pcCardId held `shouldMatchList` map pcCardId tucked
      attrs <- getsGame $ toAttrs . (HashMap.! mid) . gameMinions
      Minion.getModifiedAttack attrs `shouldReturn` 1

  it "Drang's barrage grows after successive attacks and adds that much indirect damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      mid <- addMinion you Cards.street60146
      _ <- setEncounterDeck (replicate 5 Cards.street60188) []
      resolve [MinionMessage mid $ MinionAttacks you]
      getCountersOn mid `shouldReturn` 1
      damageOf you `shouldReturn` 2
      resolve [MinionMessage mid $ MinionAttacks you]
      getCountersOn mid `shouldReturn` 2
      damageOf you `shouldReturn` 5

  it "Mysterio deals the treachery used as his boost without discarding or revealing it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      mid <- addMinion you Cards.street60147
      ([boost,_],_) <- setEncounterDeck [Cards.street60116,Cards.street60188] []
      resolve [MinionMessage mid $ MinionAttacks you]
      map ecCardId . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` [ecCardId boost]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` [ecCardId boost]

  it "Proxima Midnight can add threat instead of increasing the current attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      mid <- addMinion you Cards.street60148
      _ <- setEncounterDeck (replicate 3 Cards.street60188) []
      before <- mainSchemeThreatNow
      runNow [MinionMessage mid $ MinionAttacks you]
      choose "resolve Proxima's interrupt" (isAbilityOf mid)
      choose "place threat" (isLabel "$label.fnePlaceThreat")
      pass
      mainSchemeThreatNow `shouldReturn` (before + 1)
      damageOf you `shouldReturn` 2

  it "Imprisoned blocks basic powers and form change until its three-resource cost is paid" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      card <- revealNow you Cards.street60150
      pass
      offers (isHeroAttack you) `shouldReturn` False
      let oid = ObligationId $ unCardId $ ecCardId card
      energy <- putInHand you "01088"
      genius <- putInHand you "01089"
      choose "pay to escape" (isAbilityOf $ ObligationRef oid)
      payWith [energy,genius]
      pass
      offers (isHeroAttack you) `shouldReturn` True
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId card]
