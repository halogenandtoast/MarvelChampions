module Marvel.Treachery.Treacheries.HeartShapedHerb
  ( heartShapedHerb
  , HeartShapedHerb(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

heartShapedHerb :: TreacheryCard HeartShapedHerb
heartShapedHerb =
  treacheryWith HeartShapedHerb Cards.heartShapedHerb (surgeL .~ True)

newtype HeartShapedHerb = HeartShapedHerb (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage HeartShapedHerb where
  runMessage msg t@(HeartShapedHerb attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        villain <- selectJust ActiveVillain
        minions <- selectList $ MinionEngagedWith $ IdentityWithId identityId
        pushAll
          $ VillainMessage villain VillainBecomeTough
          : map (`MinionMessage` MinionBecomeTough) minions
        pure t
      _ -> HeartShapedHerb <$> runMessage msg attrs
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        villain <- selectJust ActiveVillain
        push $ VillainMessage villain VillainBecomeTough
        pure t
      _ -> pure t
    _ -> HeartShapedHerb <$> runMessage msg attrs
