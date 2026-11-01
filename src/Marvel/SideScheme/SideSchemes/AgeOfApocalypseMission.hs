module Marvel.SideScheme.SideSchemes.AgeOfApocalypseMission where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Data.HashMap.Strict qualified as HashMap
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype AoaMissionSideScheme = AoaMissionSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

liberateTheSeattleCoreFront :: SideSchemeCard AoaMissionSideScheme
liberateTheSeattleCoreFront = sideScheme AoaMissionSideScheme Cards.liberateTheSeattleCoreFront (PerPlayer 5)

liberateTheSeattleCoreFinished :: SideSchemeCard AoaMissionSideScheme
liberateTheSeattleCoreFinished = sideScheme AoaMissionSideScheme Cards.liberateTheSeattleCoreFinished (Static 0)

evacuateSurvivorsFront :: SideSchemeCard AoaMissionSideScheme
evacuateSurvivorsFront = sideScheme AoaMissionSideScheme Cards.evacuateSurvivorsFront (PerPlayer 5)

evacuateSurvivorsFinished :: SideSchemeCard AoaMissionSideScheme
evacuateSurvivorsFinished = sideScheme AoaMissionSideScheme Cards.evacuateSurvivorsFinished (Static 0)

sabotageTheSeaWallFront :: SideSchemeCard AoaMissionSideScheme
sabotageTheSeaWallFront = sideScheme AoaMissionSideScheme Cards.sabotageTheSeaWallFront (PerPlayer 5)

sabotageTheSeaWallFinished :: SideSchemeCard AoaMissionSideScheme
sabotageTheSeaWallFinished = sideScheme AoaMissionSideScheme Cards.sabotageTheSeaWallFinished (Static 0)

findLostMutantsFront :: SideSchemeCard AoaMissionSideScheme
findLostMutantsFront = sideScheme AoaMissionSideScheme Cards.findLostMutantsFront (PerPlayer 5)

findLostMutantsFinished :: SideSchemeCard AoaMissionSideScheme
findLostMutantsFinished = sideScheme AoaMissionSideScheme Cards.findLostMutantsFinished (Static 0)

protectTheProfessorFront :: SideSchemeCard AoaMissionSideScheme
protectTheProfessorFront = sideScheme AoaMissionSideScheme Cards.protectTheProfessorFront (PerPlayer 5)

protectTheProfessorFinished :: SideSchemeCard AoaMissionSideScheme
protectTheProfessorFinished = sideScheme AoaMissionSideScheme Cards.protectTheProfessorFinished (Static 0)

instance HasModifiersFor AoaMissionSideScheme where
  getModifiersFor _ target (AoaMissionSideScheme a) | target == toRef a = do
    mission <- getMission
    pure $ [ThreatCannotBeRemovedByThwarting]
      <> [CannotLeavePlay | maybe False (\m -> not m.missionFinished && not (null m.missionMinions)) mission]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaMissionSideScheme where
  runMessage msg e@(AoaMissionSideScheme a) = runQueueT $ case msg of
    WithDiscarded target FromDeck (onlyPlayerCards -> cards) | target == toRef a -> do
      cardSpecific GameRef "aoaMissionDiscarded" cards
      pure e
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      cancelMatchingMessage $ \case DefeatedLeavesPlay target -> target == toRef a; _ -> False
      cardSpecific GameRef "aoaMissionFinish" True
      pure e
    CardSpecific target "aoaMissionFlip" _ | target == toRef a -> do
      let code = flipCardCode $ toCardCode a
      pure $ AoaMissionSideScheme a {sideSchemeCardDef = fromMaybe (getCardDef a) (HashMap.lookup code Cards.allSideSchemes), sideSchemeThreat = 0}
    _ -> AoaMissionSideScheme <$> runMessage msg a
