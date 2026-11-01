module Marvel.Rules.DeadpoolIconsSpec (spec) where

import Marvel.Ally.Allies.SpiderManPeterParker (SpiderManPeterParker (..))
import Marvel.Ally.Types (Ally (..))
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

newtype DeadpoolIconModifiers = DeadpoolIconModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage DeadpoolIconModifiers where
  runMessage msg (DeadpoolIconModifiers attrs) = DeadpoolIconModifiers <$> runMessage msg attrs

modifyIcons :: EntityMatcher -> [Modifier] -> GameAppT ()
modifyIcons matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (DeadpoolIconModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = modifiers, effectEnds = Nothing })

spec :: Spec
spec = describe "Deadpool encounter icon counting" do
  it "counts each printed crisis icon and each gained icon with its multiplicity" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      hasCrisisInYourArea `shouldReturn` False
      first <- revealNow p SideSchemes.crowdControl
      _ <- revealNow p SideSchemes.crowdControl
      modifyIcons (SideSchemeEntity $ SideSchemeWithId $ sideSchemeOf first)
        [CrisisIcon, AccelerationIconModifier 2, AmplifyIconModifier 3, HazardIconModifier 4]
      getEncounterIconCounts `shouldReturn` (3, 2, 3, 4)
      getEncounterIconCount `shouldReturn` 12
      hasCrisisInYourArea `shouldReturn` True

  it "excludes physical acceleration tokens from encounter icon totals" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      main <- mainSchemeIdNow
      scheme <- revealNow p SideSchemes.crowdControl
      resolve
        [ PlaceAccelerationToken main
        , MainSchemeMessage main $ SetMainSchemeAcceleration 2
        , SideSchemeMessage (sideSchemeOf scheme) SideSchemePlaceAccelerationToken
        ]
      getEncounterIconCounts `shouldReturn` (1, 0, 0, 0)
      getEncounterIconCount `shouldReturn` 1

  it "counts only main-scheme and adjacent physical tokens for Deadpool's token bonuses" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      main <- mainSchemeIdNow
      scheme <- revealNow p SideSchemes.crowdControl
      modifyIcons (SideSchemeEntity $ SideSchemeWithId $ sideSchemeOf scheme)
        [AccelerationIconModifier 5]
      resolve
        [ PlaceAccelerationToken main
        , MainSchemeMessage main $ SetMainSchemeAcceleration 2
        , SideSchemeMessage (sideSchemeOf scheme) SideSchemePlaceAccelerationToken
        ]
      getMainSchemeTokenCount `shouldReturn` 3
      getEncounterIconCounts `shouldReturn` (1, 5, 0, 0)

  it "counts encounter icons printed on player cards and removes them when blanked" do
    tg <- testGame ["captain-marvel"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "04045"
      let aid = allyIdOf card
      attrs <- allyIn "04045"
      let def = (allyCardDef attrs) {cdAcceleration = 2, cdAmplify = 3, cdHazards = 4}
      withGame_ $ entitiesL . alliesL . at aid ?~ Ally (SpiderManPeterParker attrs {allyCardDef = def})
      getEncounterIconCounts `shouldReturn` (0, 2, 3, 4)
      getEncounterIconCount `shouldReturn` 9
      getAccelerationCount `shouldReturn` 2
      getAmplifyCount `shouldReturn` 3
      getHazardCount `shouldReturn` 4
      hasCrisisInYourArea `shouldReturn` False
      modifyIcons (AllyEntity $ AllyWithId aid) [Blank]
      getEncounterIconCounts `shouldReturn` (0, 0, 0, 0)
      getAccelerationCount `shouldReturn` 0
      getAmplifyCount `shouldReturn` 0
      getHazardCount `shouldReturn` 0
      modifyIcons (AllyEntity $ AllyWithId aid) [CrisisIcon]
      getEncounterIconCounts `shouldReturn` (1, 0, 0, 0)
      hasCrisisInYourArea `shouldReturn` True
