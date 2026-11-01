module Marvel.Rules.WaspAssaultSpec (spec) where

import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers
import Marvel.Keyword (Keyword (Patrol))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport

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

-- The first allocation selects a shared stat pool, following the official
-- Wasp/Befuddle analogy documented in campaign-next-evolution.md.
setupBoard :: GameAppT (IdentityId, SideSchemeId, MainSchemeId)
setupBoard = do
  you <- getYou
  clearHand you
  changeFormTo you C
  pass
  card <- encounterCard Schemes.ne40189
  resolve [PutEncounterCardIntoPlay you card]
  let side = sideSchemeOf card
  resolve [SideSchemeMessage side $ SideSchemePlaceThreat 4]
  main <- mainSchemeIdNow
  resolve [MainSchemeMessage main $ MainSchemePlaceThreat 5]
  sideSchemeThreat side `shouldReturn` 7
  mainSchemeThreatNow `shouldReturn` 5
  pure (you, side, main)

spec :: Spec
spec = describe "Giant Wasp Assault divided basic thwart" do
  it "uses the first Assault target's ATK pool across Assault and ordinary schemes" do
    tg <- testGame ["wasp"]
    inGame tg do
      (you, side, main) <- setupBoard
      modifiersOn (IdentityEntity $ IdentityWithId you) [AttackModifier 2]
      choose "basic thwart" (isHeroThwart you)
      choose "divide" (isLabel "$label.divideThreat")
      choose "first point on Assault" (isTargetChoice side)
      choose "second point on main" (isTargetChoice main)
      choose "third point on main" (isTargetChoice main)
      choose "fourth point on Assault" (isTargetChoice side)
      pass
      sideSchemeThreat side `shouldReturn` 5
      mainSchemeThreatNow `shouldReturn` 3

  it "keeps the initial ordinary target's THW pool when later allocating to Assault" do
    tg <- testGame ["wasp"]
    inGame tg do
      (you, side, main) <- setupBoard
      modifiersOn (IdentityEntity $ IdentityWithId you) [AttackModifier 2]
      choose "basic thwart" (isHeroThwart you)
      choose "divide" (isLabel "$label.divideThreat")
      choose "first point on main" (isTargetChoice main)
      choose "second point on Assault" (isTargetChoice side)
      pass
      sideSchemeThreat side `shouldReturn` 6
      mainSchemeThreatNow `shouldReturn` 4

  it "does not add a THW bonus to an ATK-selected Assault pool" do
    tg <- testGame ["wasp"]
    inGame tg do
      (you, side, main) <- setupBoard
      modifiersOn (IdentityEntity $ IdentityWithId you) [AttackModifier 2, ThwartModifier 3]
      choose "basic thwart" (isHeroThwart you)
      choose "divide" (isLabel "$label.divideThreat")
      choose "first point on Assault" (isTargetChoice side)
      choose "second point on main" (isTargetChoice main)
      choose "third point on main" (isTargetChoice main)
      choose "fourth point on Assault" (isTargetChoice side)
      pass
      sideSchemeThreat side `shouldReturn` 5
      mainSchemeThreatNow `shouldReturn` 3

  it "keeps source-restricted schemes out of later allocation choices" do
    tg <- testGame ["wasp"]
    inGame tg do
      (you, side, main) <- setupBoard
      blocked <- sideSchemeOf <$> revealNow you Schemes.bombScare
      pass
      before <- sideSchemeThreat blocked
      modifiersOn (IdentityEntity $ IdentityWithId you)
        [AttackModifier 2, CannotThwartScheme $ SchemeSideSchemeId blocked]
      choose "basic thwart" (isHeroThwart you)
      choose "divide" (isLabel "$label.divideThreat")
      offers (isTargetChoice blocked) `shouldReturn` False
      choose "first point on Assault" (isTargetChoice side)
      offers (isTargetChoice blocked) `shouldReturn` False
      choose "second point on main" (isTargetChoice main)
      offers (isTargetChoice blocked) `shouldReturn` False
      choose "third point on main" (isTargetChoice main)
      choose "fourth point on Assault" (isTargetChoice side)
      pass
      sideSchemeThreat blocked `shouldReturn` before
      sideSchemeThreat side `shouldReturn` 5
      mainSchemeThreatNow `shouldReturn` 3

  it "retains basic-thwart patrol permission for later allocations to the main scheme" do
    tg <- testGame ["wasp"]
    inGame tg do
      (you, side, main) <- setupBoard
      minion <- addMinion you Minions.hydraSoldier
      modifiersOn (MinionEntity $ MinionWithId minion) [KeywordModifier Patrol]
      modifiersOn (IdentityEntity $ IdentityWithId you) [AttackModifier 2, BasicThwartIgnoresPatrol]
      choose "basic thwart" (isHeroThwart you)
      choose "divide" (isLabel "$label.divideThreat")
      choose "first point on Assault" (isTargetChoice side)
      offers (isTargetChoice main) `shouldReturn` True
      choose "second point on main" (isTargetChoice main)
      choose "third point on main" (isTargetChoice main)
      choose "fourth point on Assault" (isTargetChoice side)
      pass
      sideSchemeThreat side `shouldReturn` 5
      mainSchemeThreatNow `shouldReturn` 3

  it "uses THW when the initially selected scheme's Assault is blanked" do
    tg <- testGame ["wasp"]
    inGame tg do
      (you, side, main) <- setupBoard
      modifiersOn (IdentityEntity $ IdentityWithId you) [AttackModifier 2]
      modifiersOn (SideSchemeEntity $ SideSchemeWithId side) [Blank]
      choose "basic thwart" (isHeroThwart you)
      choose "divide" (isLabel "$label.divideThreat")
      choose "first point on blanked Assault" (isTargetChoice side)
      choose "second point on main" (isTargetChoice main)
      pass
      sideSchemeThreat side `shouldReturn` 6
      mainSchemeThreatNow `shouldReturn` 4
