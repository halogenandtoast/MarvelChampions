module Marvel.SideScheme.SideSchemes.Nc48028 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.SideScheme.Types
import Marvel.SideScheme.Cards qualified as Cards

nc48028 :: SideSchemeCard Nc48028
nc48028 = sideScheme Nc48028 Cards.nc48028 (Static 5)

newtype Nc48028 = Nc48028 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48028 where
  runMessage msg (Nc48028 attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- maybe getActivePlayerId pure (sideSchemeDefeatedBy attrs)
      azazel <- selectOne $ MinionIs Minions.nc48027
      case azazel of
        Just mid -> cardSpecific GameRef "ncDealMinionAsEncounter" (mid, you)
        Nothing -> do
          cards <- findEncounterCards (CardIs Minions.nc48027)
          for_ (headMay cards) $ \card -> do
            deck <- getEncounterDeck
            cardSpecific GameRef "ncDealFoundEncounter" (you, card)
            when (card `elem` deck) shuffleEncounterDeck
      Nc48028 <$> runMessage msg attrs
    _ -> Nc48028 <$> runMessage msg attrs
