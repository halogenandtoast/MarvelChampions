module Marvel.SideScheme.SideSchemes.AgeOfApocalypseApocalypse where

import Marvel.Prelude
import Marvel.Attachment.Cards ()
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Scenario.AgeOfApocalypseApocalypse
import Marvel.Trait qualified as Trait
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaApocalypseSideScheme = AoaApocalypseSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

heartOfTheEmpire :: SideSchemeCard AoaApocalypseSideScheme
heartOfTheEmpire = sideScheme AoaApocalypseSideScheme Cards.heartOfTheEmpire (Static 2)

theToweringCitadel :: SideSchemeCard AoaApocalypseSideScheme
theToweringCitadel = sideScheme AoaApocalypseSideScheme Cards.theToweringCitadel (Static 3)

theTyrantsThrone :: SideSchemeCard AoaApocalypseSideScheme
theTyrantsThrone = sideScheme AoaApocalypseSideScheme Cards.theTyrantsThrone (Static 4)

theApocalypseSolution :: SideSchemeCard AoaApocalypseSideScheme
theApocalypseSolution = sideScheme (\a -> AoaApocalypseSideScheme a {sideSchemeCrisis = True}) Cards.theApocalypseSolution (Static 3)

instance HasModifiersFor AoaApocalypseSideScheme where
  getModifiersFor _ target (AoaApocalypseSideScheme attrs) | isTarget attrs target, toCardCode attrs /= "45111" = do
    prelate <- selectAny $ MinionWithTrait Trait.Prelate
    pure [ThreatCannotBeRemoved | prelate]
  getModifiersFor _ _ _ = pure []
instance RunMessage AoaApocalypseSideScheme where
  runMessage msg a@(AoaApocalypseSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      if toCardCode attrs == "45111" then apocalypseScalarHp >>= discardTopOfEncounterDeck else do
        first <- selectJust FirstPlayer
        players <- getPlayers
        revealRandomPrelate first
        for_ (filter (/= first) players) dealEncounterCard
        case toCardCode attrs of
          "45104a" -> do
            cancelMatchingMessage $ \case
              DefeatedLeavesPlay target -> isTarget attrs target
              _ -> False
            sideSchemeStep sid SideSchemePlaceInitialThreat
          "45104b" -> do
            searchEverywhereAndRevealEncounterCard first $ CardIs Cards.theTyrantsThrone
            removeFromGame attrs
          "45105a" -> transformSideSchemeToAttachment sid "45105b" first
          _ -> pure ()
      pure $ if toCardCode attrs == "45104a" then buildCard theToweringCitadel sid else a
    _ -> AoaApocalypseSideScheme <$> runMessage msg attrs
