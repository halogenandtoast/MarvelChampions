module Marvel.Aspect.X23PackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (find)
import Marvel.Ally.Types qualified as Ally
import Marvel.Keyword (Keyword (Assault))
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Modifier
import Marvel.Query
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

asX23 :: IdentityId -> GameAppT ()
asX23 p = changeForm p >> pass >> clearHand p

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

playCard :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playCard p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pure card

specialize :: IdentityId -> CardCode -> GameAppT PlayerCard
specialize p code = do
  aside <- getSetAsidePlayerCards
  let card = fromMaybe (error "missing linked specialization") $ find ((== code) . toCardCode) aside
  training <- playCard p "43021" ["01091"]
  let sid = SideSchemeId $ unCardId $ pcCardId training
  start [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 100]
  pressWhenRevealed
  choose "specialization" (isTargetChoice card)
  pass
  pure card

victoryScheme :: IdentityId -> GameAppT ()
victoryScheme p = do
  scheme <- playCard p "43039" []
  let sid = SideSchemeId $ unCardId $ pcCardId scheme
  resolve [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 100]

spec :: Spec
spec = describe "X23 pack" do
  it "Boom Boom stores counters, remains exhausted after declining, then detonates for the total" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      boom <- playCard p "43013" ["01090", "01091"]
      choose "charge" (cardAbility boom 1)
      choose "keep Boom Boom" (isLabel "$label.done")
      pass
      getCountersOn (allyIdOf boom) `shouldReturn` 1
      offers (cardAbility boom 1) `shouldReturn` False
      resolve [AllyMessage (allyIdOf boom) ReadiedAlly]
      minion <- addMinion p Minions.hydraSoldier
      choose "charge again" (cardAbility boom 1)
      choose "detonate" (isLabel "$label.detonateBoomBoom")
      pass
      villainDamageNow `shouldReturn` 2
      minionDamageOf minion `shouldReturn` Just 2
      alliesWith "43013" `shouldReturn` []
      map toCardCode <$> discardOf p >>= (`shouldSatisfy` ("43013" `elem`))

  it "Rictor discards the top card after attacking and damages each engaged minion per printed resource" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg
      asX23 p
      rictor <- playCard p "43014" ["01090", "01091"]
      own <- addMinion p Minions.radioactiveMan
      other <- addMinion (player2 tg) Minions.hydraSoldier
      top <- stackDeck p ["01088"]
      choose "attack" (cardAbility rictor 301)
      chooseVillain
      choose "Rictor response" (cardAbility rictor 1)
      pass
      minionDamageOf own `shouldReturn` Just 2
      minionDamageOf other `shouldReturn` Just 0
      villainDamageNow `shouldReturn` 2
      discard <- discardOf p
      map pcCardId discard `shouldSatisfy` (pcCardId (head top) `elem`)

  it "Rictor's star does not trigger after thwarting" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      rictor <- playCard p "43014" ["01090", "01091"]
      placeMainThreat 3
      choose "thwart" (cardAbility rictor 300)
      chooseMainScheme
      offers (cardAbility rictor 1) `shouldReturn` False

  it "Shatterstar engages a minion without receiving the already-engaged bonus" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg
      asX23 p
      star <- playCard p "43015" ["01090", "01089"]
      minion <- addMinion (player2 tg) Minions.hydraSoldier
      choose "attack" (cardAbility star 301)
      choose "other player's minion" (isTargetChoice minion)
      choose "engage" (cardAbility star 1)
      pass
      selectAny (MinionWithId minion <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True
      minionDamageOf minion `shouldReturn` Just 2

  it "Shatterstar gets one extra attack only when already engaged with the attacked minion" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      star <- playCard p "43015" ["01090", "01089"]
      minion <- addMinion p Minions.hydraSoldier
      choose "attack" (cardAbility star 301)
      choose "engaged minion" (isTargetChoice minion)
      choose "attack bonus" (cardAbility star 1)
      pass
      minionDamageOf minion `shouldReturn` Just 3

  it "Shatterstar's interrupt is unavailable against a villain" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      star <- playCard p "43015" ["01090", "01089"]
      choose "attack" (cardAbility star 301)
      chooseVillain
      offers (cardAbility star 1) `shouldReturn` False

  it "Keep Them Busy uses ATK for basic thwarts, enters victory, and clears five threat per player" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg
      asX23 p
      placeMainThreat 12
      scheme <- playCard p "43018" ["01091"]
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      choose "basic thwart" (isHeroThwart p)
      choose "assault scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 5
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 5]
      mainSchemeThreatNow `shouldReturn` 2
      map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` ["43018"]

  it "Now I'm Mad has a strict threshold based on printed starting HP, including HP bonuses" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      mad <- putInHand p "43019"
      payment <- putInHand p "01091"
      playOn mad p [payment]
      setDamage p 5
      getModifiers (toRef p) >>= (`shouldSatisfy` (AttackModifier 1 `notElem`))
      setDamage p 6
      mods <- getModifiers (toRef p)
      mods `shouldSatisfy` (AttackModifier 1 `elem`)
      mods `shouldSatisfy` (ThwartPenalty 1 `elem`)
      endurance <- putInHand p "43027"
      resource <- putInHand p "01091"
      playOn endurance p [resource]
      getModifiers (toRef p) >>= (`shouldSatisfy` (AttackModifier 1 `notElem`))
      setDamage p 9
      getModifiers (toRef p) >>= (`shouldSatisfy` (AttackModifier 1 `elem`))
      second <- putInHand p "43019"
      offers (isPlay second) `shouldReturn` False

  it "Now I'm Mad can be controlled by another player and does not modify alter ego" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asX23 p
      card <- putInHand p "43019"
      payment <- putInHand p "01091"
      playOn card other [payment]
      setDamage other 7
      getModifiers (toRef other) >>= (`shouldSatisfy` (AttackModifier 1 `notElem`))
      getsGame (fmap (upgradeController . toAttrs) . HashMap.lookup (upgradeIdOf card) . gameUpgrades) `shouldReturn` Just other

  it "The Direct Approach grants assault to its side scheme and permits only one attached copy" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      sid <- sideSchemeOf <$> revealNow p Schemes.crowdControl
      pass
      approach <- putInHand p "43020"
      payment <- putInHand p "01091"
      playOn approach sid [payment]
      schemeHasKeyword (SchemeSideSchemeId sid) Assault `shouldReturn` True
      another <- putInHand p "43020"
      offers (isPlay another) `shouldReturn` False
      choose "basic thwart" (isHeroThwart p)
      choose "assault scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 1

  it "Specialized Training grants an owned linked upgrade and excludes already specialized players" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43034"
      getsGame (fmap (upgradeOwner . toAttrs) . HashMap.lookup (upgradeIdOf specialist) . gameUpgrades) `shouldReturn` Just p
      second <- playCard p "43021" ["01091"]
      let sid = SideSchemeId $ unCardId $ pcCardId second
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 5]
      length <$> upgradesOf p `shouldReturn` 2
      length <$> getSetAsidePlayerCards >>= (`shouldSatisfy` (>= 3))

  it "Combat Specialist adds hero ATK and draws once after a basic attack" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43034"
      clearHand p
      choose "basic attack" (isHeroAttack p)
      chooseVillain
      choose "specialist" (cardAbility specialist 1)
      pass
      villainDamageNow `shouldReturn` 2
      length <$> handOf p `shouldReturn` 1
      upgradeIsExhausted (upgradeIdOf specialist) `shouldReturn` True

  it "Surveillance Specialist adds hero THW and draws after a basic thwart" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43037"
      clearHand p
      placeMainThreat 5
      choose "basic thwart" (isHeroThwart p)
      chooseMainScheme
      choose "specialist" (cardAbility specialist 1)
      pass
      mainSchemeThreatNow `shouldReturn` 2
      length <$> handOf p `shouldReturn` 1

  it "Defense Specialist adds hero DEF and draws after a basic defense" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43035"
      clearHand p
      villain <- villainIdNow
      start [VillainMessage villain (VillainAttacksWithoutBoost p)]
      choose "defend" isDefend
      choose "specialist" (cardAbility specialist 1)
      pass
      damageOf p `shouldReturn` 0
      length <$> handOf p `shouldReturn` 1

  it "Front Line Specialist applies +4 HP in either form and draws after enemy attack damage" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43036"
      hitPointsOf p `shouldReturn` 14
      resolve [IdentityMessage p $ ChangedToForm B]
      hitPointsOf p `shouldReturn` 14
      resolve [IdentityMessage p $ ChangedToForm A]
      clearHand p
      villain <- villainIdNow
      start [VillainMessage villain (VillainAttacksWithoutBoost p)]
      choose "undefended" isNoDefender
      choose "specialist" (cardAbility specialist 1)
      pass
      length <$> handOf p `shouldReturn` 1

  it "Front Line Specialist does not respond to ability damage" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43036"
      start [IdentityMessage p (IdentityDamaged GameRef $ toDamage 1 FromAbility)]
      offers (cardAbility specialist 1) `shouldReturn` False

  it "Critical Hit is gated by victory and stuns the enemy after a hero's basic attack" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      hit <- putInHand p "43016"
      payment <- putInHand p "01088"
      choose "attack" (isHeroAttack p)
      chooseVillain
      offers (isPlay hit) `shouldReturn` False
      pass
      resolve [IdentityMessage p ReadiedIdentity]
      victoryScheme p
      choose "attack again" (isHeroAttack p)
      chooseVillain
      playWith hit [payment]
      pass
      villain <- villainIdNow
      selectAny (EnemyWithId (EnemyVillainId villain) <> StunnedEnemy) `shouldReturn` True

  it "Predictable Ploy cancels an encounter-deck treachery after victory" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      victoryScheme p
      ploy <- putInHand p "43038"
      payment <- putInHand p "01089"
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.advance
      playWith ploy [payment]
      pass
      mainSchemeThreatNow `shouldReturn` before

  it "Predictable Ploy is unavailable without victory and for a discard-pile reveal" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      ploy <- putInHand p "43038"
      _ <- putInHand p "01089"
      _ <- revealNow p Treacheries.advance
      offers (isPlay ploy) `shouldReturn` False
      pass
      victoryScheme p
      card <- encounterCard Treacheries.advance
      start [RevealEncounterCardFrom W.RevealedFromDiscardPile p card]
      offers (isPlay ploy) `shouldReturn` False

  it "Anticipated Attack gives tough when the attack begins and prevents its damage" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      victoryScheme p
      attack <- putInHand p "43040"
      payment <- putInHand p "01090"
      villain <- villainIdNow
      start [VillainMessage villain (VillainAttacksWithoutBoost p)]
      playWith attack [payment]
      pass
      damageOf p `shouldReturn` 0

  it "Anticipated Attack cannot be played without a victory side scheme" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      attack <- putInHand p "43040"
      _ <- putInHand p "01090"
      villain <- villainIdNow
      start [VillainMessage villain (VillainAttacksWithoutBoost p)]
      offers (isPlay attack) `shouldReturn` False

  it "Rally the Troops heals each player's allies and enters victory" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg
      asX23 p
      own <- putIntoPlay p "01051"
      other <- putIntoPlay (player2 tg) "01050"
      resolve [AllyMessage (allyIdOf own) (AllyDamaged GameRef $ toDamage 2 FromAbility), AllyMessage (allyIdOf other) (AllyDamaged GameRef $ toDamage 3 FromAbility)]
      scheme <- playCard p "43039" []
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 6]
      allyDamage <$> allyIn "01051" `shouldReturn` 0
      allyDamage <$> allyIn "01050" `shouldReturn` 1
      map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` ["43039"]

  it "Combat Specialist ignores attack events" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43034"
      event <- putInHand p "01005"
      payment <- putInHand p "01088"
      extra <- putInHand p "01091"
      playWith event [payment, extra]
      chooseVillain
      offers (cardAbility specialist 1) `shouldReturn` False

  it "Defense Specialist ignores a defense event without a basic defense" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43035"
      attack <- putInHand p "43040"
      payment <- putInHand p "01090"
      villain <- villainIdNow
      start [VillainMessage villain (VillainAttacksWithoutBoost p)]
      playWith attack [payment]
      pass
      upgradeIsExhausted (upgradeIdOf specialist) `shouldReturn` False

  it "Critical Hit cannot trigger after an ally attack" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      victoryScheme p
      ally <- playCard p "43014" ["01090", "01091"]
      hit <- putInHand p "43016"
      _ <- putInHand p "01088"
      choose "ally attacks" (cardAbility ally 301)
      chooseVillain
      whenM (offers $ cardAbility ally 1) $ choose "decline Rictor response" isNoResponse
      offers (isPlay hit) `shouldReturn` False

  it "Anticipated Attack cannot give another tough card to an already tough hero" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      victoryScheme p
      resolve [IdentityMessage p IdentityBecomeTough]
      attack <- putInHand p "43040"
      _ <- putInHand p "01090"
      villain <- villainIdNow
      start [VillainMessage villain (VillainAttacksWithoutBoost p)]
      offers (isPlay attack) `shouldReturn` False

  it "Anticipated Attack can defend an attack initiated against another player's alter ego" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asX23 p
      victoryScheme p
      attack <- putInHand p "43040"
      payment <- putInHand p "01090"
      villain <- villainIdNow
      start [VillainMessage villain (VillainAttacksWithoutBoost other)]
      playWith attack [payment]
      pass
      damageOf p `shouldReturn` 0
      damageOf other `shouldReturn` 0

  it "The Direct Approach cannot attach to a permanent side scheme" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      _ <- revealNow p Schemes.consumeTheWorld
      pass
      approach <- putInHand p "43020"
      _ <- putInHand p "01091"
      offers (isPlay approach) `shouldReturn` False

  it "Specialized Training lets every eligible player choose a distinct linked upgrade" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asX23 p
      aside <- getSetAsidePlayerCards
      let findCard code = fromMaybe (error "no linked upgrade") $ find ((== code) . toCardCode) aside
          combat = findCard "43034"
          surveillance = findCard "43037"
      training <- playCard p "43021" ["01091"]
      let sid = SideSchemeId $ unCardId $ pcCardId training
      start [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 10]
      pressWhenRevealed
      choose "first player chooses who resolves first" (isTargetChoice p)
      choose "first specialization" (isTargetChoice combat)
      offers (isTargetChoice combat) `shouldReturn` False
      choose "second specialization" (isTargetChoice surveillance)
      pass
      getsGame (fmap (upgradeController . toAttrs) . HashMap.lookup (upgradeIdOf combat) . gameUpgrades) `shouldReturn` Just p
      getsGame (fmap (upgradeController . toAttrs) . HashMap.lookup (upgradeIdOf surveillance) . gameUpgrades) `shouldReturn` Just other

  it "Rictor counts a discarded wild twice while controlled by Domino in hero form" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      rictor <- playCard p "43014" ["01090", "01091"]
      minion <- addMinion p Minions.radioactiveMan
      _ <- stackDeck p ["40040"]
      choose "Rictor attacks" (cardAbility rictor 301)
      chooseVillain
      choose "Rictor response" (cardAbility rictor 1)
      pass
      minionDamageOf minion `shouldReturn` Just 2

  it "Defense Specialist still draws when the attacking minion is defeated by retaliate" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43035"
      _ <- playCard p "43009" ["01091"]
      minion <- addMinion p Minions.hydraSoldier
      resolve [MinionMessage minion (MinionDamaged GameRef $ toDamage 3 FromAbility)]
      clearHand p
      start [MinionMessage minion (MinionAttacks p)]
      choose "defend" isDefend
      choose "specialist after retaliate" (cardAbility specialist 1)
      pass
      minionDamageOf minion `shouldReturn` Nothing
      length <$> handOf p `shouldReturn` 1

  it "Surveillance Specialist ignores thwart events" do
    tg <- testGame ["x-23"]
    inGame tg do
      let p = player1 tg
      asX23 p
      specialist <- specialize p "43037"
      placeMainThreat 5
      event <- putInHand p "01060"
      payment <- putInHand p "01090"
      playWith event [payment]
      chooseMainScheme
      offers (cardAbility specialist 1) `shouldReturn` False
      upgradeIsExhausted (upgradeIdOf specialist) `shouldReturn` False

  it "Specialized Training lets the first player resolve the second player's choice first" do
    tg <- testGame ["x-23", "spider-man"]
    inGame tg do
      let p = player1 tg; other = player2 tg
      asX23 p
      aside <- getSetAsidePlayerCards
      let findCard code = fromMaybe (error "no linked upgrade") $ find ((== code) . toCardCode) aside
          combat = findCard "43034"
          defense = findCard "43035"
      training <- playCard p "43021" ["01091"]
      let sid = SideSchemeId $ unCardId $ pcCardId training
      start [SideSchemeMessage sid $ SideSchemeThwarted (toSource p) 10]
      pressWhenRevealed
      fmap fst <$> asked `shouldReturn` Just p
      offers (isTargetChoice p) `shouldReturn` True
      choose "resolve second player first" (isTargetChoice other)
      fmap fst <$> asked `shouldReturn` Just other
      choose "second player claims Combat Specialist" (isTargetChoice combat)
      fmap fst <$> asked `shouldReturn` Just p
      offers (isTargetChoice combat) `shouldReturn` False
      choose "first player claims Defense Specialist" (isTargetChoice defense)
      pass
      getsGame (fmap (upgradeController . toAttrs) . HashMap.lookup (upgradeIdOf combat) . gameUpgrades) `shouldReturn` Just other
      getsGame (fmap (upgradeController . toAttrs) . HashMap.lookup (upgradeIdOf defense) . gameUpgrades) `shouldReturn` Just p
