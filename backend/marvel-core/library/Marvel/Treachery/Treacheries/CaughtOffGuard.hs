module Marvel.Treachery.Treacheries.CaughtOffGuard (
  caughtOffGuard,
  CaughtOffGuard (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

caughtOffGuard :: TreacheryCard CaughtOffGuard
caughtOffGuard = treachery CaughtOffGuard Cards.caughtOffGuard

newtype CaughtOffGuard = CaughtOffGuard (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage CaughtOffGuard where
  runMessage msg t@(CaughtOffGuard attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        supports <-
          selectRefs $
            SupportControlledBy $
              IdentityWithId
                identityId
        upgrades <-
          selectRefs $
            UpgradeControlledBy $
              IdentityWithId
                identityId
        if null supports && null upgrades
          then pure . CaughtOffGuard $ attrs & surgeL .~ True
          else do
            chooseOne identityId $
              [ TargetLabel support [DiscardTarget support]
              | support <- supports
              ]
                <> [ TargetLabel upgrade [DiscardTarget upgrade]
                   | upgrade <- upgrades
                   ]
            pure t
      _ -> CaughtOffGuard <$> runMessage msg attrs
    _ -> CaughtOffGuard <$> runMessage msg attrs
