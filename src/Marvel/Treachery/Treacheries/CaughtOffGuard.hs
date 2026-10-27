module Marvel.Treachery.Treacheries.CaughtOffGuard (
  caughtOffGuard,
  caughtOffGuardMuseum,
  CaughtOffGuard (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Keyword (Keyword (Permanent))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

caughtOffGuard :: TreacheryCard CaughtOffGuard
caughtOffGuard = treachery CaughtOffGuard Cards.caughtOffGuard

-- | Infiltrate the Museum's copy, the same card.
caughtOffGuardMuseum :: TreacheryCard CaughtOffGuard
caughtOffGuardMuseum = treachery CaughtOffGuard Cards.caughtOffGuardMuseum

newtype CaughtOffGuard = CaughtOffGuard (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage CaughtOffGuard where
  runMessage msg t@(CaughtOffGuard attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        supports <- selectRefs $ SupportControlledBy $ IdentityWithId identityId
        upgrades <- selectRefs $ UpgradeControlledBy $ IdentityWithId identityId
        -- a permanent card cannot be discarded (the Milano): with only those, none is
        g <- getGame
        let discardable = filter (maybe True (Permanent `notElem`) . fmap cdKeywords . abilityCardDef g) (supports <> upgrades)
        if null discardable
          then pure . CaughtOffGuard $ attrs & surgeL .~ True
          else do
            chooseOne identityId [targeting c (removeFromPlay c) | c <- discardable]
            pure t
      _ -> CaughtOffGuard <$> runMessage msg attrs
    _ -> CaughtOffGuard <$> runMessage msg attrs
