module Marvel.Treachery.Treacheries.InColdBlood (inColdBlood, InColdBlood (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

data Meta = Meta
  { pending :: Maybe (MinionId, IdentityId)
  , attackMade :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype InColdBlood = InColdBlood (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

inColdBlood :: TreacheryCard InColdBlood
inColdBlood = treachery (InColdBlood . (`With` Meta Nothing False)) Cards.inColdBlood

instance IsTreachery InColdBlood where
  toTreacheryAttrs (InColdBlood (attrs `With` _)) = attrs
  treacheryModifiersFor _ (IdentityRef you) (InColdBlood (_ `With` meta)) =
    pure [CannotPlayCards (CardWithType EventType) | Just (_, target) <- [pending meta], you == target]
  treacheryModifiersFor _ _ _ = pure []

instance RunMessage InColdBlood where
  runMessage msg t@(InColdBlood (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      minions <- selectList (MinionIs Minions.theLizard)
      case minions of
        [] -> pure $ InColdBlood ((attrs & surgeL .~ True) `With` meta)
        mid : _ -> do
          enemyAttacks mid you
          cardSpecific attrs "afterAttack" ()
          pure $ InColdBlood (attrs `With` Meta (Just (mid, you)) False)
    CheckWindows windows | Just (mid, you) <- pending meta
      , any (\case W.Window W.When (W.EnemyAttack (EnemyMinionId enemy) target) -> enemy == mid && target == you; _ -> False) windows ->
          pure $ InColdBlood (attrs `With` meta {attackMade = True})
    CardSpecific (isTarget attrs -> True) "afterAttack" _ ->
      pure $ InColdBlood ((attrs & surgeL %~ (|| not (attackMade meta))) `With` Meta Nothing False)
    _ -> InColdBlood . (`With` meta) <$> runMessage msg attrs
