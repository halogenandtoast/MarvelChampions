{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Treachery.Treacheries.Fw53042 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Keyword (Keyword(Piercing))
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Treachery.Types

import Marvel.Hood.Helpers (activationWillResolve)
import Marvel.FalconWinter.Helpers
fw53042 :: TreacheryCard Fw53042
fw53042 = treachery Fw53042 Cards.fw53042

newtype Fw53042 = Fw53042 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw53042 where
  runMessage msg a@(Fw53042 attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      findAndActivateMinion you attrs Cards.fw53038
      pure a
    CardSpecific (isTarget attrs -> True) "fwActivateFoundMinion" (fromJSON -> Success (you, def :: CardDef)) -> do
      found <- selectOne $ MinionIs def
      case found of
        Nothing -> pure $ Fw53042 attrs {treacherySurge = True}
        Just mid -> do
          resolves <- activationWillResolve you (EnemyMinionId mid)
          activateEnemy (EnemyMinionId mid) you
          unless resolves $ gainSurge attrs
          pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      attack <- getEnemyAttack enemy
      for_ attack $ \_ -> attackGains enemy Piercing
      pure a
    _ -> Fw53042 <$> runMessage msg attrs
