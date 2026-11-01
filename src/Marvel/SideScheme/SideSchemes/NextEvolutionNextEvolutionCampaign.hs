module Marvel.SideScheme.SideSchemes.NextEvolutionNextEvolutionCampaign where

import Marvel.Prelude
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.Id
import Marvel.Query
import Marvel.Matchers
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NextEvolutionCampaignSideScheme = NextEvolutionCampaignSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40190a :: SideSchemeCard NextEvolutionCampaignSideScheme
ne40190a = sideScheme NextEvolutionCampaignSideScheme Cards.ne40190a (PerPlayer 4)

ne40191a :: SideSchemeCard NextEvolutionCampaignSideScheme
ne40191a = sideScheme NextEvolutionCampaignSideScheme Cards.ne40191a (PerPlayer 4)

ne40192a :: SideSchemeCard NextEvolutionCampaignSideScheme
ne40192a = sideScheme NextEvolutionCampaignSideScheme Cards.ne40192a (PerPlayer 4)

ne40193a :: SideSchemeCard NextEvolutionCampaignSideScheme
ne40193a = sideScheme NextEvolutionCampaignSideScheme Cards.ne40193a (PerPlayer 4)

ne40194a :: SideSchemeCard NextEvolutionCampaignSideScheme
ne40194a = sideScheme NextEvolutionCampaignSideScheme Cards.ne40194a (PerPlayer 4)

ne40195a :: SideSchemeCard NextEvolutionCampaignSideScheme
ne40195a = sideScheme NextEvolutionCampaignSideScheme Cards.ne40195a (PerPlayer 4)

ne40202 :: SideSchemeCard NextEvolutionCampaignSideScheme
ne40202 = sideScheme NextEvolutionCampaignSideScheme Cards.ne40202 (Static 6)

instance RunMessage NextEvolutionCampaignSideScheme where
  runMessage msg e@(NextEvolutionCampaignSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a, toCardCode a /= "40202" -> do
      first <- selectJust FirstPlayer
      let back = fromMaybe (error "missing NeXt Evolution reward environment") $
            lookup (flipCardCode $ toCardCode a) Environments.allEnvironments
          card = MkEncounterCard (CardId $ unSideSchemeId sid) back
      -- This physical card flips instead of entering the victory display. It is
      -- still a defeated player side scheme for abilities responding to defeat.
      removeFromPlay sid
      putEncounterCardIntoPlay first card
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "40202" -> do
      dealBoost enemy
      pure e
    _ -> NextEvolutionCampaignSideScheme <$> runMessage msg a
