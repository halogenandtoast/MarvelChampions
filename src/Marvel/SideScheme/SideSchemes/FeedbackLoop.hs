module Marvel.SideScheme.SideSchemes.FeedbackLoop where

import Marvel.Id (IdentityId)
import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Minion.Minions.Zzzax (controlledEnergy, handEnergy)
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

feedbackLoop :: SideSchemeCard FeedbackLoop
feedbackLoop = sideScheme FeedbackLoop Cards.feedbackLoop (Static 2)

newtype FeedbackLoop = FeedbackLoop (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FeedbackLoop where
  runMessage msg s@(FeedbackLoop attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      cardSpecific attrs "resolvePlayers" players
      pure s
    CardSpecific (isTarget attrs -> True) "resolvePlayers" (fromJSON -> Success remaining) -> do
      players <- getPlayers
      for_ (headMay players) $ \first -> when (notNull (remaining :: [IdentityId])) $
        chooseOrRunOne first
          [targeting you do
             cardSpecific attrs "forPlayer" you
             cardSpecific attrs "resolvePlayers" (filter (/= you) remaining)
          | you <- remaining, you `elem` players]
      pure s
    CardSpecific (isTarget attrs -> True) "forPlayer" (fromJSON -> Success you) -> do
      n <- (+) <$> handEnergy you <*> controlledEnergy you
      when (n > 0) $ placeThreat (sideSchemeId attrs) n
      pure s
    _ -> FeedbackLoop <$> runMessage msg attrs
