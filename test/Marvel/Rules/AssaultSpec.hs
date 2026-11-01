module Marvel.Rules.AssaultSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Keyword (Keyword (Assault))
import TestImport

allyDamageOf :: AllyId -> GameAppT (Maybe Natural)
allyDamageOf aid = getsGame (fmap (Ally.allyDamage . toAttrs) . HashMap.lookup aid . gameAllies)

newtype AssaultTestModifiers = AssaultTestModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage AssaultTestModifiers where
  runMessage msg (AssaultTestModifiers attrs) = AssaultTestModifiers <$> runMessage msg attrs

modifiersOn :: EntityMatcher -> [Modifier] -> GameAppT ()
modifiersOn matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (AssaultTestModifiers EffectAttrs
    { effectId = eid
    , effectCardCode = "01039"
    , effectSource = GameRef
    , effectMatcher = matcher
    , effectModifiers = modifiers
    , effectEnds = Nothing
    })

assaultScheme :: IdentityId -> GameAppT SideSchemeId
assaultScheme you = do
  card <- encounterCard Schemes.ne40189
  resolve [PutEncounterCardIntoPlay you card]
  let sid = sideSchemeOf card
  resolve [SideSchemeMessage sid $ SideSchemePlaceThreat 4]
  sideSchemeThreat sid `shouldReturn` 7
  pure sid

spec :: Spec
spec = describe "Assault ally basic powers" do
  it "lets dash-THW Hulk thwart Assault using numeric ATK and attack consequential damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "01050"
      sid <- assaultScheme you
      hulk <- allyIn "01050"
      choose "Hulk basic thwart" (isAllyThwart hulk)
      choose "Assault scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 4
      allyDamageOf (allyIdOf card) `shouldReturn` Just 1
      -- This was a thwart, so Hulk's after-attack response never triggers.
      offers (isAbilityOfCard card) `shouldReturn` False

  it "offers Hulk only the Assault target when an ordinary scheme also has threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01050"
      sid <- assaultScheme you
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      hulk <- allyIn "01050"
      choose "Hulk basic thwart" (isAllyThwart hulk)
      offers (isTargetChoice main) `shouldReturn` False
      offers (isTargetChoice sid) `shouldReturn` True

  it "does not allow dash-THW Hulk to initiate an ordinary thwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01050"
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main $ MainSchemePlaceThreat 3]
      hulk <- allyIn "01050"
      offers (isAllyThwart hulk) `shouldReturn` False

  it "does not treat Red House's optional interrupt as constant Assault" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01050"
      _ <- revealNow you Schemes.theRedHouse
      pass
      hulk <- allyIn "01050"
      offers (isAllyThwart hulk) `shouldReturn` False

  it "applies ATK bonuses to an Assault basic thwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "01050"
      sid <- assaultScheme you
      modifiersOn (AllyEntity $ AllyWithId $ allyIdOf card) [AttackModifier 2]
      hulk <- allyIn "01050"
      choose "Hulk basic thwart" (isAllyThwart hulk)
      choose "Assault scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 2
      allyDamageOf (allyIdOf card) `shouldReturn` Just 1

  it "ignores THW bonuses when ATK replaces THW for Assault" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "01050"
      sid <- assaultScheme you
      modifiersOn (AllyEntity $ AllyWithId $ allyIdOf card) [ThwartModifier 5]
      hulk <- allyIn "01050"
      choose "Hulk basic thwart" (isAllyThwart hulk)
      choose "Assault scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 4

  it "uses Black Cat's zero attack consequence instead of her one thwart consequence" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "01002"
      sid <- assaultScheme you
      blackCat <- allyIn "01002"
      choose "Black Cat basic thwart" (isAllyThwart blackCat)
      choose "Assault scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 6
      allyDamageOf (allyIdOf card) `shouldReturn` Just 0

  it "lets confused Hulk attempt an otherwise legal Assault thwart without removing threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "01050"
      sid <- assaultScheme you
      resolve [GiveStatus (AllyRef $ allyIdOf card) ConfusedStatus]
      hulk <- allyIn "01050"
      choose "Hulk attempts Assault thwart" (isAllyThwart hulk)
      pass
      sideSchemeThreat sid `shouldReturn` 7
      after <- allyIn "01050"
      Ally.allyConfused after `shouldBe` False
      Ally.allyExhausted after `shouldBe` True
      allyDamageOf (allyIdOf card) `shouldReturn` Just 0

  it "requires a legal Assault target for confused dash-THW Hulk" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "01050"
      sid <- assaultScheme you
      modifiersOn (AllyEntity $ AllyWithId $ allyIdOf card) [CannotThwartScheme $ SchemeSideSchemeId sid]
      resolve [GiveStatus (AllyRef $ allyIdOf card) ConfusedStatus]
      hulk <- allyIn "01050"
      offers (isAllyThwart hulk) `shouldReturn` False
      Ally.allyConfused hulk `shouldBe` True
      Ally.allyExhausted hulk `shouldBe` False

  it "uses an ordinary ally's THW and thwart consequence while Assault is blanked" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- putIntoPlay you "33013"
      sid <- assaultScheme you
      modifiersOn (SideSchemeEntity $ SideSchemeWithId sid) [Blank]
      rockslide <- allyIn "33013"
      choose "Rockslide basic thwart" (isAllyThwart rockslide)
      choose "blanked scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 6
      allyDamageOf (allyIdOf card) `shouldReturn` Just 1

  it "does not give dash-THW Hulk permission to thwart a blanked Assault scheme" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01050"
      sid <- assaultScheme you
      modifiersOn (SideSchemeEntity $ SideSchemeWithId sid) [Blank]
      hulk <- allyIn "01050"
      offers (isAllyThwart hulk) `shouldReturn` False

  it "uses a hero's THW instead of ATK while Assault is blanked" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      sid <- assaultScheme you
      modifiersOn (SideSchemeEntity $ SideSchemeWithId sid) [Blank]
      choose "hero basic thwart" $ \case
        UseAbility a -> abilitySource a == IdentityRef you && abilityIndex a == 300
        _ -> False
      choose "blanked scheme" (isTargetChoice sid)
      pass
      sideSchemeThreat sid `shouldReturn` 6

  it "honors external Assault gains and losses while printed text is blanked" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01050"
      sid <- assaultScheme you
      modifiersOn (SideSchemeEntity $ SideSchemeWithId sid) [Blank, KeywordModifier Assault]
      hulk <- allyIn "01050"
      offers (isAllyThwart hulk) `shouldReturn` True
      modifiersOn (SideSchemeEntity $ SideSchemeWithId sid) [LoseKeyword Assault]
      offers (isAllyThwart hulk) `shouldReturn` False
