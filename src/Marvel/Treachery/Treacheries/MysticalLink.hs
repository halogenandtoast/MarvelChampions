module Marvel.Treachery.Treacheries.MysticalLink (
  mysticalLink,
  MysticalLink (..),
) where

import Marvel.Prelude

import Control.Monad ()
import Data.Maybe (maybeToList)
import Data.Aeson ()
import Marvel.Attack ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Data.List ()
import Marvel.Id
import Marvel.Identity.Types ()
import Marvel.Projection ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

mysticalLink :: TreacheryCard MysticalLink
mysticalLink = treachery MysticalLink Cards.mysticalLink

newtype MysticalLink = MysticalLink (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: place 2 threat on each side scheme.
-- [star] Boost: Wrecker gets +3 ATK for this activation unless you place 2 threat on his
-- side scheme: asked only of his attack, where the ATK counts.
instance RunMessage MysticalLink where
  runMessage msg t@(MysticalLink attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      schemes <- selectList AnySideScheme
      for_ schemes \s -> placeThreat s 2
      pure t
    Boost (RevealedAsBoost target enemy@(EnemyVillainId v)) | isTarget attrs target -> do
      wrecker <- villainNamed "Wrecker"
      attacking <- isJust <$> getVillainAttack v
      when (wrecker == Just v && attacking) do
        you <- getBoostedPlayer enemy
        scheme <- signatureSchemeOf v
        chooseOne you $
          labeled (countLabel "wreckerGetsAttack" 3) (addBoost v 3)
            : [labeled (countLabel "placeThreatOnHisSideScheme" 2) (placeThreat s 2) | s <- maybeToList scheme]
      pure t
    _ -> MysticalLink <$> runMessage msg attrs
