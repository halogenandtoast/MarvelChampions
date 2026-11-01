module Marvel.SideScheme.SideSchemes.AgeOfApocalypseCampaign where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newtype AoaCampaignSideScheme = AoaCampaignSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

northAmericanSeaWall :: SideSchemeCard AoaCampaignSideScheme
northAmericanSeaWall = sideScheme AoaCampaignSideScheme Cards.northAmericanSeaWall (Static 2)

instance HasModifiersFor AoaCampaignSideScheme where
  getModifiersFor _ (VillainRef _) _ = pure [CannotTakeDamage]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaCampaignSideScheme where
  runMessage msg e@(AoaCampaignSideScheme a) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      dealBoostAsEncounterCard a you
      pure e
    _ -> AoaCampaignSideScheme <$> runMessage msg a
