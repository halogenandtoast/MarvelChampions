module Marvel.Encounter.CableNemesisSpec (spec) where

import Data.Maybe (fromJust)
import Data.HashMap.Strict qualified as HashMap
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Modifier
import TestImport
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types (obligationSideScheme)


asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

_cardAbility :: PlayerCard -> Natural -> Choice -> Bool
_cardAbility card n choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == n
  _ -> False

purge :: GameAppT (Maybe SideSchemeId)
purge = getsGame $ headMay . HashMap.keys . HashMap.filter ((== "40006") . toCardCode . getCardDef) . gameSideSchemes


-- Individual card tests explicitly arrange Purge rather than depend on the
-- setup search's first offered card or on the shuffled opening hand.
cableGame :: [Text] -> IO TestGame
cableGame heroes = do
  tg <- testGame heroes
  inGame tg do
    current <- purge
    when (isNothing current) do
      sides <- getsGame $ HashMap.keys . HashMap.filter (isJust . SideScheme.sideSchemePlayerCard . toAttrs) . gameSideSchemes
      resolve [RemoveFromPlay $ SideSchemeRef sid | sid <- sides]
      _ <- putIntoPlay (player1 tg) "40006"
      pure ()
  pure tg

spec :: Spec
spec = describe "Cable nemesis" do
  it "Stryfe has villainous and cancels a Psionic event while damaging himself" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      stryfe <- addMinion p Minions.cable40032
      card <- putInHand p "40005"
      pay <- putInHand p "01088"
      extra <- putInHand p "01091"
      playWith card [pay,extra]
      choose "Stryfe's forced interrupt" (isAbilityOf stryfe)
      pass
      villainDamageNow `shouldReturn` 0
      minionDamageOf stryfe `shouldReturn` Just 1

  it "Stryfe does not cancel a non-Psionic event" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.cable40032
      card <- putInHand p "01087"
      pay <- putInHand p "01088"
      playWith card [pay]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 3

  it "nemesis Mind Scan places two threat with no victory side schemes" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      before <- mainSchemeThreatNow
      _ <- revealNow p Treacheries.cable40035
      pass
      mainSchemeThreatNow `shouldReturn` before + 2

  it "nemesis Telekinetic Blast deals two damage with no victory side schemes" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      before <- damageOf p
      _ <- revealNow p Treacheries.cable40036
      pass
      damageOf p `shouldReturn` before + 2

  it "nemesis Telekinetic Blast counts completed player side schemes" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      purge >>= traverse_ (\sid -> resolve [AddToVictoryDisplay (SideSchemeRef sid)])
      before <- damageOf p
      _ <- revealNow p Treacheries.cable40036
      pass
      damageOf p `shouldReturn` before + 3

  it "Back to the Future restricts Cable to its scheme and his engaged minions" do
    tg <- cableGame ["cable", "spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- revealNow p Schemes.cable40033
      let sid = sideSchemeOf scheme
      mods <- getModifiers (IdentityRef p)
      mods `shouldSatisfy` (\actual -> all (`elem` actual) [CanOnlyRemoveThreatFrom $ SchemeSideSchemeId sid, CanOnlyDamageEngagedEnemies])
      modsOther <- getModifiers (IdentityRef $ player2 tg)
      modsOther `shouldSatisfy` (\actual -> all (`elem` actual) [CannotRemoveThreatFrom $ SchemeSideSchemeId sid, CannotDamageEnemiesEngagedWith p])

  it "Technovirus Resurgence attaches to Purge already in play" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      sid <- fromJust <$> purge
      _ <- revealNow p Obligations.cable40031
      pass
      attached <- getsGame $ map (obligationSideScheme . toAttrs) . HashMap.elems . gameObligations
      attached `shouldBe` [Just sid]

  it "Technovirus Resurgence removes Purge's victory bonus when returning it" do
    tg <- cableGame ["cable"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- fromJust <$> purge
      resolve [AddToVictoryDisplay (SideSchemeRef sid)]
      _ <- revealNow p Obligations.cable40031
      pass
      map toCardCode <$> getPlayerVictoryDisplay `shouldReturn` []
      sideSchemeThreat sid `shouldReturn` 5
      currentIdentity <$> getPlayer p >>= \case
        HeroSide hero -> Hero.getModifiedThwart (toAttrs hero) `shouldReturn` 2
        _ -> error "expected Cable"
