module Marvel.Treachery.Treacheries.SliceAndDice (sliceAndDice, SliceAndDice (..)) where

import Marvel.Prelude
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

data Meta = Meta
  { pendingEnemy :: Maybe MinionId
  , attackMade :: Bool
  , identityDamagedByAttack :: Maybe IdentityId
  , defeatedCharacter :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SliceAndDice = SliceAndDice (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

sliceAndDice :: TreacheryCard SliceAndDice
sliceAndDice = treachery (SliceAndDice . (`With` Meta Nothing False Nothing False)) Cards.sliceAndDice

instance IsTreachery SliceAndDice where
  toTreacheryAttrs (SliceAndDice (attrs `With` _)) = attrs

instance RunMessage SliceAndDice where
  runMessage msg (SliceAndDice (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      prowler <- selectOne (MinionIs Minions.prowler)
      case prowler of
        Nothing -> pure $ SliceAndDice ((attrs & surgeL .~ True) `With` meta)
        Just mid -> do
          targets <- selectList (IdentityWithFewestRemainingHitPoints AnyIdentity)
          unless (null targets) $ chooseOrRunOne you
            [targeting target (enemyAttacks mid target) | target <- targets]
          cardSpecific attrs "afterAttack" ()
          pure $ SliceAndDice (attrs `With` Meta (Just mid) False Nothing False)
    CheckWindows windows | Just mid <- pendingEnemy meta
      , any (\case W.Window W.When (W.EnemyAttack (EnemyMinionId enemy) _) -> enemy == mid; _ -> False) windows ->
          pure $ SliceAndDice (attrs `With` meta {attackMade = True})
    AllyMessage _ (AllyDefeatedByAttack (EnemyMinionId enemy))
      | pendingEnemy meta == Just enemy && attackMade meta ->
          pure $ SliceAndDice (attrs `With` meta {defeatedCharacter = True})
    IdentityMessage you (IdentityDamaged (MinionRef enemy) damage)
      | pendingEnemy meta == Just enemy && attackMade meta && damageSource damage == FromAttack && damageAmount damage > 0 ->
          pure $ SliceAndDice (attrs `With` meta {identityDamagedByAttack = Just you})
    IdentityMessage you IdentityDefeated
      | identityDamagedByAttack meta == Just you ->
          pure $ SliceAndDice (attrs `With` meta {defeatedCharacter = True})
    CardSpecific (isTarget attrs -> True) "afterAttack" _ ->
      pure $ SliceAndDice ((attrs & surgeL %~ (|| not (attackMade meta) || defeatedCharacter meta)) `With` Meta Nothing False Nothing False)
    _ -> SliceAndDice . (`With` meta) <$> runMessage msg attrs
