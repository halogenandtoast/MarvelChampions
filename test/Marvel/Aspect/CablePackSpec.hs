module Marvel.Aspect.CablePackSpec (spec) where

import Marvel.Trait (Trait (..))
import Data.HashMap.Strict qualified as HashMap
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Modifier
import Marvel.Query
import Marvel.Matchers
import TestImport


asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

_cardAbility :: PlayerCard -> Natural -> Choice -> Bool
_cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

_purge :: GameAppT (Maybe SideSchemeId)
_purge = getsGame $ headMay . HashMap.keys . HashMap.filter ((== "40006") . toCardCode . getCardDef) . gameSideSchemes


spec :: Spec
spec = describe "Cable pack" do
  it "Caliban discards through an X-Force ally and returns that ally to hand" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- stackDeck p ["01091", "40024", "01088"]
      caliban <- putInHand p "40014"
      pay <- putInHand p "01088"
      extra <- putInHand p "01091"
      playWith caliban [pay, extra]
      choose "Caliban" (isAbilityOfCard caliban)
      pass
      map toCardCode <$> handOf p `shouldReturn` ["40024"]
      map toCardCode <$> discardOf p >>= (\codes -> codes `shouldSatisfy` elem "01091")

  it "Fantomex searches E.V.A. into play without paying its cost" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      eva <- testHead <$> stackDeck p ["40021"]
      fantomex <- putInHand p "40015"
      e <- putInHand p "01088"
      m <- putInHand p "01089"
      playWith fantomex [e,m]
      choose "Fantomex" (isAbilityOfCard fantomex)
      choose "E.V.A." (isTargetChoice eva)
      pass
      selectAny (SupportNamed $ cdName $ lookupPlayerCard "40021") `shouldReturn` True

  it "E.V.A. is discarded immediately without Fantomex" do
    tg <- testGame ["cable"]
    inGame tg do
      eva <- putIntoPlay (player1 tg) "40021"
      getsGame (HashMap.member (supportOf eva) . gameSupports) `shouldReturn` False

  it "Sunspot's damage counts energy resources actually spent" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sunspot <- putInHand p "40016"
      e <- putInHand p "01088"
      extra <- putInHand p "01091"
      playWith sunspot [e,extra]
      choose "Sunspot" (isAbilityOfCard sunspot)
      choose "player" (isTargetChoice p)
      pass
      villainDamageNow `shouldReturn` 2

  it "Mission Planning cannot be played with an empty victory display" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      mission <- putInHand p "40017"
      _ <- putInHand p "01088"
      offers (isPlay mission) `shouldReturn` False

  it "Deadpool heals three instead of being defeated by consequential damage" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- putIntoPlay p "40024"
      let aid = allyIdOf card
      resolve [AllyMessage aid $ AllyDamaged (IdentityRef p) (toDamage 2 FromAbility)]
      deadpoolAttrs <- allyIn "40024"
      choose "Deadpool attack" (isAllyAttack deadpoolAttrs)
      chooseVillain
      pass
      alliesWith "40024" >>= (\allies -> map allyDamage allies `shouldBe` [0])
      getsGame (sum . map areaAccelerationTokens . gameAreas) `shouldReturn` 1

  it "Deadpool is defeated normally by non-consequential damage" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      card <- putIntoPlay p "40024"
      resolve [AllyMessage (allyIdOf card) $ AllyDamaged (IdentityRef p) (toDamage 3 FromAbility)]
      alliesWith "40024" `shouldReturn` []

  it "Deathlok can attach Sidearm from a discard pile without playing it" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sidearm <- putInHand p "40030"
      resolve [DiscardedCard $ PlayerCard sidearm]
      deathlok <- putInHand p "40025"
      e <- putInHand p "01088"
      m <- putInHand p "01089"
      playWith deathlok [e,m]
      choose "Deathlok" (isAbilityOfCard deathlok)
      choose "attach Sidearm" (isTargetChoice sidearm)
      pass
      getModifiers (AllyRef $ allyIdOf deathlok) >>= (\mods -> mods `shouldSatisfy` elem (AttackModifier 1))

  it "Deathlok ignores Honorary Avenger's play restriction but retains attach restrictions" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      honorary <- putInHand p "03025"
      skyCycle <- putInHand p "04015"
      training <- putInHand p "33015"
      resolve [DiscardedCard $ PlayerCard card | card <- [honorary, skyCycle, training]]
      deathlok <- putInHand p "40025"
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      playWith deathlok [energy, genius]
      choose "Deathlok" (isAbilityOfCard deathlok)
      offers (isTargetChoice skyCycle) `shouldReturn` False
      offers (isTargetChoice training) `shouldReturn` False
      choose "attach Honorary Avenger" (isTargetChoice honorary)
      pass
      selectAny (AllyWithId (allyIdOf deathlok) <> AllyWithTrait Avenger) `shouldReturn` True

  it "Frenemies chooses both targets before removing threat and requires different schemes" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      old <- getsGame $ HashMap.keys . gameSideSchemes
      resolve [RemoveFromPlay $ SideSchemeRef sid | sid <- old]
      _ <- putIntoPlay p "40024"
      bomb <- revealNow p Schemes.bombScare
      pass
      let side = sideSchemeOf bomb
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      before <- sideSchemeThreat side
      frenemies <- putInHand p "40026"
      pay <- putInHand p "01091"
      playWith frenemies [pay]
      choose "first scheme" (isTargetChoice side)
      sideSchemeThreat side `shouldReturn` before
      mainSchemeThreatNow `shouldReturn` 3
      offers (isTargetChoice side) `shouldReturn` False
      offers (isLabel "$label.done") `shouldReturn` False
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 0
      getsGame (HashMap.member side . gameSideSchemes) `shouldReturn` False
      damageOf p `shouldReturn` 1
      allyIn "40024" >>= \ally -> allyDamage ally `shouldBe` 1

  it "Frenemies cannot gain a main-scheme target by clearing its only initial crisis scheme" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      old <- getsGame $ HashMap.keys . gameSideSchemes
      resolve [RemoveFromPlay $ SideSchemeRef sid | sid <- old]
      _ <- putIntoPlay p "40024"
      crisis <- revealNow p Schemes.ne40146
      pass
      let side = sideSchemeOf crisis
      resolve [SideSchemeMessage side $ SideSchemeThreatRemoved (IdentityRef p) 1]
      sideSchemeThreat side `shouldReturn` 3
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      frenemies <- putInHand p "40026"
      pay <- putInHand p "01091"
      playWith frenemies [pay]
      offers (isTargetChoice main) `shouldReturn` False
      choose "only initial legal scheme" (isTargetChoice side)
      pass
      getsGame (HashMap.member side . gameSideSchemes) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` 3
      offers (isTargetChoice main) `shouldReturn` False
      offers (== EndTurn) `shouldReturn` True

  it "Frenemies cannot be played without Deadpool in play" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      frenemies <- putInHand p "40026"
      _ <- putInHand p "01091"
      offers (isPlay frenemies) `shouldReturn` False

  it "The Power of the Mind pays two resources for a Psionic card" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      scan <- putInHand p "40003"
      power <- putInHand p "40028"
      sid <- mainSchemeIdNow
      resolve [MainSchemeMessage sid $ MainSchemePlaceThreat 3]
      playWith scan [power]
      choose "main" (isTargetChoice sid)
      pass
      mainSchemeThreatNow `shouldReturn` 0

  it "Psimitar cannot respond to its own entry" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      psimitar <- putInHand p "40029"
      pay <- putInHand p "01088"
      playWith psimitar [pay]
      offers (isAbilityOfCard psimitar) `shouldReturn` False

  it "Hope Summers gains the identity's current traits" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      hope <- putIntoPlay p "40204"
      selectAny (AllyWithId (allyIdOf hope) <> AllyWithTrait XForce) `shouldReturn` True
      selectAny (AllyWithId (allyIdOf hope) <> AllyWithTrait Soldier) `shouldReturn` True

  it "Sidearm's new printing uses the existing ranged attack bonus" do
    tg <- testGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      deadpool <- putIntoPlay p "40024"
      sidearm <- putInHand p "40030"
      pay <- putInHand p "01091"
      playOn sidearm (allyIdOf deadpool) [pay]
      pass
      getModifiers (AllyRef $ allyIdOf deadpool) >>= (\mods -> mods `shouldSatisfy` elem (AttackModifier 1))

  it "Establish Perimeter grants every identity a tough status when defeated" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      scheme <- putIntoPlay (player1 tg) "40020"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      resolve [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      for_ [player1 tg, player2 tg] \p ->
        playerIdentityTough . toAttrs <$> getPlayer p `shouldReturn` True

  it "Call for Backup can put a high-cost ally into play for free" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- stackDeck p ["40025"]
      scheme <- putIntoPlay p "40018"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      choose "search" (isLabel "$label.searchDeck")
      choose "Deathlok" (\case TargetLabel (CardIdRef _) _ -> True; _ -> False)
      pass
      alliesWith "40025" >>= (\allies -> length allies `shouldBe` 1)

  it "Build Support offers each player an optional support search" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      scheme <- putIntoPlay p "40027"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      offers (isLabel "$label.searchDeck") `shouldReturn` True
      choose "decline search" (isLabel "$label.done")
      pass
      map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` ["40027"]

  it "Lock and Load attaches the searched Sidearm to a legal ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      ally <- putIntoPlay p "40024"
      sidearm <- testHead <$> stackDeck p ["40030"]
      scheme <- putIntoPlay p "40019"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      choose "search" (isLabel "$label.searchDeck")
      choose "Sidearm" (isTargetChoice sidearm)
      pass
      getsGame (fmap (upgradeAttachedAlly . toAttrs) . HashMap.lookup (upgradeOf sidearm) . gameUpgrades) `shouldReturn` Just (Just $ allyIdOf ally)
      getModifiers (AllyRef $ allyIdOf ally) >>= \mods -> mods `shouldSatisfy` elem (AttackModifier 1)

  it "Lock and Load does not offer Sidearm when no ally can receive it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      sidearm <- testHead <$> stackDeck p ["40030"]
      scheme <- putIntoPlay p "40019"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      choose "search" (isLabel "$label.searchDeck")
      offers (isTargetChoice sidearm) `shouldReturn` False
      getsGame (HashMap.member (upgradeOf sidearm) . gameUpgrades) `shouldReturn` False

  it "Build Support excludes a unique support whose title is already in play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "40007"
      duplicate <- testHead <$> stackDeck p ["40007"]
      scheme <- putIntoPlay p "40027"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      choose "search" (isLabel "$label.searchDeck")
      offers (isTargetChoice duplicate) `shouldReturn` False
      getsGame (HashMap.member (supportOf duplicate) . gameSupports) `shouldReturn` False

  it "Call for Backup excludes an ally whose unique title is already in play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "40024"
      duplicate <- testHead <$> stackDeck p ["40024"]
      scheme <- putIntoPlay p "40018"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      choose "search" (isLabel "$label.searchDeck")
      offers (isTargetChoice duplicate) `shouldReturn` False
      alliesWith "40024" >>= \allies -> length allies `shouldBe` 1

  it "Lock and Load offers a Weapon search and can be declined" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      scheme <- putIntoPlay p "40019"
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      runNow [SideSchemeMessage sid DefeatSideScheme, DefeatedLeavesPlay (SideSchemeRef sid)]
      offers (isLabel "$label.searchDeck") `shouldReturn` True
      choose "decline" (isLabel "$label.done")
      pass
      map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` ["40019"]
