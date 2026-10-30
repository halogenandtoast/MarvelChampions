-- | Ronan the Accuser's side schemes (The Galaxy's Most Wanted, scenario 5).
module Marvel.SideScheme.SideSchemes.Ronan (
  cutThePower,
  CutThePower (..),
  pincerManeuver,
  PincerManeuver (..),
  superiorTactics,
  SuperiorTactics (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id (CharacterId (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

cutThePower :: SideSchemeCard CutThePower
cutThePower = sideSchemeWith CutThePower Cards.cutThePower (PerPlayer 3) (crisisL .~ True)

-- | A crisis icon.
newtype CutThePower = CutThePower (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Boost: choose to either exhaust the Milano or place 2 threat on the main
-- scheme; the Milano only while it is in play and ready (RR Choose).
instance RunMessage CutThePower where
  runMessage msg s@(CutThePower attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      ready <- milanoIsReady
      chooseOrRunOne you $
        [labeled "$label.exhaustTheMilano" exhaustMilano | ready]
          <> [labeled (countLabel "placeThreatOnMainScheme" 2) (selectOne EncounterMainScheme >>= traverse_ (`placeThreat` 2))]
      pure s
    _ -> CutThePower <$> runMessage msg attrs

-- | Base threat 3; Hinder 2 per player and its two acceleration icons are its card's.
pincerManeuver :: SideSchemeCard PincerManeuver
pincerManeuver = sideScheme PincerManeuver Cards.pincerManeuver (Static 3)

newtype PincerManeuver = PincerManeuver (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | First Player Action: exhaust the Milano → remove 3 threat from this scheme.
instance IsSideScheme PincerManeuver where
  sideSchemeAbilities (PincerManeuver a) =
    [ability a 1 Action (SelfMatches FirstPlayer) ExhaustMilanoCost $ runAbility a 1]

instance RunMessage PincerManeuver where
  runMessage msg s@(PincerManeuver attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> s <$ thwartScheme (sideSchemeId attrs) attrs 3
    _ -> PincerManeuver <$> runMessage msg attrs

superiorTactics :: SideSchemeCard SuperiorTactics
superiorTactics = sideScheme SuperiorTactics Cards.superiorTactics (PerPlayer 2)

newtype SuperiorTactics = SuperiorTactics (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The Power Stone cannot be unattached from Ronan the Accuser.
instance HasModifiersFor SuperiorTactics where
  getModifiersFor _ (AttachmentRef aid) _ = do
    stone <- getPowerStone
    holder <- getPowerStoneHolder
    pure [CannotBeUnattached | stone == Just aid, Just (VillainCharacter _) <- [holder]]
  getModifiersFor _ _ _ = pure []

-- | When Revealed: attach the Power Stone to Ronan the Accuser; if it is already attached
-- to him, place 1 per player threat here.
instance RunMessage SuperiorTactics where
  runMessage msg s@(SuperiorTactics attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      holder <- getPowerStoneHolder
      selectOne ActiveVillain >>= traverse_ \villain ->
        if holder == Just (VillainCharacter villain)
          then placeThreat ident . fromIntegral =<< fromGameValue (PerPlayer 1)
          else attachPowerStoneTo villain
      pure s
    _ -> SuperiorTactics <$> runMessage msg attrs
