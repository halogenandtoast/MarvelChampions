module Marvel.Rules.DeadpoolSchemeDestinationSpec (spec) where

import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as SideSchemes
import TestImport

newtype SchemeDestination = SchemeDestination (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage SchemeDestination where
  runMessage msg (SchemeDestination attrs) = SchemeDestination <$> runMessage msg attrs

spec :: Spec
spec = describe "Deadpool minion scheme destinations" do
  it "places modified SCH on the specified side scheme and falls back when it leaves play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      minion <- addMinion you Minions.hydraBomber
      scheme <- revealNow you SideSchemes.crowdControl
      let sid = sideSchemeOf scheme
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect (SchemeDestination EffectAttrs
        { effectId = eid, effectCardCode = "01039", effectSource = GameRef
        , effectMatcher = MinionEntity $ MinionWithId minion
        , effectModifiers = [MinionSchemesOn sid, SchemeModifier 2], effectEnds = Nothing })
      before <- sideSchemeThreat sid
      mainBefore <- mainSchemeThreatNow
      resolve [MinionMessage minion MinionSchemes]
      sideSchemeThreat sid `shouldReturn` before + 3
      mainSchemeThreatNow `shouldReturn` mainBefore
      resolve [RemoveFromPlay $ SideSchemeRef sid]
      resolve [MinionMessage minion MinionSchemes]
      mainSchemeThreatNow `shouldReturn` mainBefore + 3
