module Marvel.Upgrade.Upgrades.AgeOfApocalypseBasicCampaign where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

newtype AoaBasicCampaignUpgrade = AoaBasicCampaignUpgrade (Attrs Upgrade)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

desperateMeasures :: UpgradeCard AoaBasicCampaignUpgrade
desperateMeasures = upgrade AoaBasicCampaignUpgrade Cards.desperateMeasures

instance IsUpgrade AoaBasicCampaignUpgrade where
  upgradeTargets _ = do
    ordinary <- selectList (NotAlly $ AllyWithUpgrade $ UpgradeIs Cards.desperateMeasures)
    mission <- getMission
    let atMission = maybe [] (.missionAllies) mission
    possible <- filterM (\aid -> not <$> selectAny (UpgradeIs Cards.desperateMeasures <> UpgradeOnAlly (AllyWithId aid))) atMission
    pure $ Just $ map AllyRef $ ordinary <> filter (`notElem` ordinary) possible

instance HasModifiersFor AoaBasicCampaignUpgrade where
  getModifiersFor _ (AllyRef aid) (AoaBasicCampaignUpgrade a) | upgradeAttachedAlly a == Just aid = pure [ThwartModifier 1, AttackModifier 1, HitPointModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaBasicCampaignUpgrade where
  runMessage msg (AoaBasicCampaignUpgrade a) = AoaBasicCampaignUpgrade <$> runMessage msg a
