module Marvel.SideScheme.SideSchemes.AMessOfThings (
  aMessOfThings,
  AMessOfThings (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

aMessOfThings :: SideSchemeCard AMessOfThings
aMessOfThings = sideScheme AMessOfThings Cards.aMessOfThings (Static 2)

newtype AMessOfThings = AMessOfThings (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: 2 more threat here for each stunned friendly character.
instance RunMessage AMessOfThings where
  runMessage msg s@(AMessOfThings attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      stunned <- selectListCount $ CharacterControlledBy AnyIdentity <> CharacterWithStatus StunnedStatus
      when (stunned > 0) $ placeThreat ident (2 * stunned)
      pure s
    _ -> AMessOfThings <$> runMessage msg attrs
