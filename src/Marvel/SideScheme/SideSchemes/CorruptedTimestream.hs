module Marvel.SideScheme.SideSchemes.CorruptedTimestream (
  corruptedTimestream,
  CorruptedTimestream (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

corruptedTimestream :: SideSchemeCard CorruptedTimestream
corruptedTimestream = sideScheme CorruptedTimestream Cards.corruptedTimestream (Static 2)

newtype CorruptedTimestream = CorruptedTimestream (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Players cannot trigger "Alter-Ego Action" abilities on obligations: those in its
-- game area.
instance HasModifiersFor CorruptedTimestream where
  getModifiersFor _ target@(ObligationRef _) (CorruptedTimestream a) = do
    here <- getAreaOfTarget (sideSchemeId a)
    there <- getAreaOfTarget target
    pure [CannotTriggerAlterEgoActions | here == there]
  getModifiersFor _ _ _ = pure []

-- | When Revealed: each player must either discard 1 random card from hand, or place 2
-- threat here.
instance RunMessage CorruptedTimestream where
  runMessage msg (CorruptedTimestream attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      attrs' <- runMessage msg attrs
      players <- getPlayers
      for_ players \p -> do
        hand <- projectMap PlayerIdentityHand unHand p
        chooseOrRunOne p $
          [labeled (countLabel "discardRandomCardsFromHand" 1) (discardFrom p RandomFromHand 1) | notNull hand]
            <> [labeled (countLabel "placeThreatOnCorruptedTimestream" 2) (placeThreat (sideSchemeId attrs) 2)]
      pure $ CorruptedTimestream attrs'
    _ -> CorruptedTimestream <$> runMessage msg attrs
