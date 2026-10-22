module Marvel.Treachery.Treacheries.HeartShapedHerb (
  heartShapedHerb,
  HeartShapedHerb (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

heartShapedHerb :: TreacheryCard HeartShapedHerb
heartShapedHerb =
  treacheryWith HeartShapedHerb Cards.heartShapedHerb (surgeL .~ True)

newtype HeartShapedHerb = HeartShapedHerb (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage HeartShapedHerb where
  runMessage msg t@(HeartShapedHerb attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        villain <- selectJust ActiveVillain
        minions <- selectList $ MinionEngagedWith $ IdentityWithId identityId
        enemyBecomesTough villain
        for_ minions enemyBecomesTough
        pure t
      _ -> HeartShapedHerb <$> runMessage msg attrs
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        enemyBecomesTough =<< selectJust ActiveVillain
        pure t
      _ -> pure t
    _ -> HeartShapedHerb <$> runMessage msg attrs
