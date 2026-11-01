module Marvel.Encounter.MagnetoNemesisSpec (spec) where

import Marvel.Ability qualified as Ability
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Matchers
import Marvel.Query
import Marvel.Villain.Types (villainTough)
import Marvel.Message.Lifted qualified as L
import TestImport

settle :: GameAppT ()
settle = do
  pass
  q <- asked
  case q >>= (headMay . filter forced . snd) of
    Just c -> choose "mandatory ability" (== c) >> settle
    Nothing -> pure ()
 where
  forced = \case
    UseAbility a -> abilityType a `elem` [Ability.ForcedResponse, Ability.ForcedInterrupt, Ability.WhenRevealed]
    _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> settle >> clearHand p

isPull :: IdentityId -> Choice -> Bool
isPull p = \case
  UseAbility a -> abilitySource a == toSource p && abilityIndex a == 1
  _ -> False

spec :: Spec
spec = describe "Magneto nemesis and Hellfire" do
  it "Old Grievances deals damage for every card discarded by Magnetic Pull" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Obligations.oldGrievances
      settle
      _ <- stackDeck p ["01086", "49011"]
      choose "Magnetic Pull" (isPull p)
      settle
      damageOf p `shouldReturn` 2

  it "Old Grievances can be discarded by exhausting Erik" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Obligations.oldGrievances
      settle
      choose "remove Old Grievances" (\case UseAbility a -> abilitySource a == toSource (ObligationRef $ ObligationId (unCardId $ ecCardId card)) && abilityIndex a == 2; _ -> False)
      settle
      exhaustedNow p `shouldReturn` True
      encounterDiscard >>= (`shouldSatisfy` elem card)

  it "Fabian Cortez discards four cards when defeated" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.fabianCortez
      top <- stackDeck p ["01086", "01087", "49002", "49003"]
      card <- putInHand p "49009"
      payment <- traverse (putInHand p) ["01088", "01086"]
      playWith card payment
      choose "defeat Fabian" (isTargetChoice enemy)
      settle
      discarded <- discardOf p
      all (`elem` discarded) top `shouldBe` True

  it "Martyr for Mutants discards nine cards from the defeating player's deck" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- sideSchemeOf <$> revealNow p Schemes.martyrForMutants
      settle
      top <- stackDeck p (replicate 9 "01086")
      resolve [SideSchemeMessage scheme (SideSchemeThreatRemoved (IdentityRef p) 3)]
      settle
      discarded <- discardOf p
      all (`elem` discarded) top `shouldBe` True

  it "Angry Acolyte finds and reveals an Acolyte when none is in play" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Treacheries.advance, Minions.fabianCortez] []
      _ <- revealNow p Treacheries.angryAcolyte
      settle
      selectAny (MinionIs Minions.fabianCortez) `shouldReturn` True

  it "The Inner Circle counts Hellfire cards when adding its revealed threat" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Minions.hellfirePawnMagneto
      scheme <- sideSchemeOf <$> revealNow p Schemes.theInnerCircle
      settle
      sideSchemeThreat scheme `shouldReturn` 6

  it "Sebastian Shaw survives a tough-blocked attack, receives a boost and cannot be attacked again" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.sebastianShaw
      choose "Magneto attacks" (isHeroAttack p)
      choose "attack Shaw" (isTargetChoice enemy)
      settle
      resolve [IdentityMessage p ReadiedIdentity]
      choose "attack again" (isHeroAttack p)
      offers (isTargetChoice enemy) `shouldReturn` False

  it "Selene cannot be attacked by allies" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      enemy <- addMinion p Minions.selene
      ally <- putIntoPlay p "49012"
      choose "M attacks" (\case UseAbility a -> abilitySource a == toSource (allyIdOf ally) && abilityIndex a == 301; _ -> False)
      offers (isTargetChoice enemy) `shouldReturn` False

  it "Hellfire Pawn prevents attacking the villain with guard and thwarting the main scheme with patrol" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.hellfirePawnMagneto
      offers (isHeroThwart p) `shouldReturn` False
      choose "attack" (isHeroAttack p)
      villain <- villainIdNow
      offers (isTargetChoice villain) `shouldReturn` False

  it "Power and Decadence gives the villain tough and becomes its boost card" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.powerAndDecadence
      settle
      villainTough . toAttrs <$> getVillain `shouldReturn` True

  it "Angry Acolyte searches when a stunned Acolyte's attack is replaced" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.fabianCortez
      resolve [MinionMessage mid $ MinionStunned GameRef]
      _ <- setEncounterDeck [Minions.frenzy, Treacheries.advance] []
      _ <- revealNow p Treacheries.angryAcolyte
      settle
      selectAny (MinionIs Minions.frenzy) `shouldReturn` True

  it "Exodus discards cards equal to its total boosted ATK" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.exodusMagneto
      top <- stackDeck p $ replicate 4 "01086"
      _ <- setEncounterDeck [Treacheries.explosion, Treacheries.assault] []
      runNow [MinionMessage mid $ MinionAttacks p]
      settle
      discarded <- discardOf p
      all (`elem` discarded) top `shouldBe` True

  it "Frenzy discards two cards after attacking" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      mid <- addMinion p Minions.frenzy
      top <- stackDeck p ["01086", "01087"]
      runNow [MinionMessage mid $ MinionAttacks p]
      settle
      discarded <- discardOf p
      all (`elem` discarded) top `shouldBe` True

  it "Power and Decadence moves its physical card to boosts without also discarding it" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Treacheries.powerAndDecadence
      settle
      discarded <- encounterDiscard
      card `elem` discarded `shouldBe` False

  it "a stunned replacement does not leak no-boost suppression into the next activation" do
    tg <- testGame ["magneto"]
    inGame tg do
      let p = player1 tg
      asHero p
      villain <- villainIdNow
      resolve [VillainMessage villain $ VillainStunned GameRef]
      L.runQueueT (L.capture $ L.enemyActivatesWithoutBoost GameRef villain p) >>= start
      settle
      _ <- setEncounterDeck [Treacheries.explosion, Treacheries.assault] []
      runNow [VillainMessage villain $ VillainAttacks p]
      settle
      damageOf p `shouldReturn` 4

  it "Fabian's when-defeated penalty follows another player's Cell Phone basic attack" do
    tg <- testGame ["magneto", "spider-man"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      asHero p
      phone <- putIntoPlay p "47019"
      m <- putIntoPlay q "49012"
      enemy <- addMinion p Minions.fabianCortez
      top <- stackDeck q $ replicate 4 "01086"
      choose "Cell Phone" (isAbilityOfCard phone)
      choose "other player" (isTargetChoice q)
      choose "M makes the basic attack" (isTargetChoice $ allyIdOf m)
      choose "M attacks" (\case UseAbility a -> abilitySource a == toSource (allyIdOf m) && abilityIndex a == 301; _ -> False)
      choose "defeat Fabian" (isTargetChoice enemy)
      settle
      discarded <- discardOf q
      all (`elem` discarded) top `shouldBe` True
