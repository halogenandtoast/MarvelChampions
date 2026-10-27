module Marvel.Treachery.Treacheries.IWillDestroyYou (
  iWillDestroyYou,
  IWillDestroyYou (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

iWillDestroyYou :: TreacheryCard IWillDestroyYou
iWillDestroyYou = treachery (IWillDestroyYou . (`With` Meta Nothing)) Cards.iWillDestroyYou

-- | Yotat's attack that gets +1 ATK, until it is declared or ends.
newtype Meta = Meta {boosted :: Maybe (MinionId, IdentityId)}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype IWillDestroyYou = IWillDestroyYou (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery IWillDestroyYou where
  toTreacheryAttrs (IWillDestroyYou (attrs `With` _)) = attrs

-- | When Revealed (Alter-Ego): this card gains surge. (Hero): Yotat the Destroyer attacks
-- you with +1 ATK. If no attack was made this way (no Yotat, or a stunned one, who only
-- discards his stun), the villain attacks you.
instance RunMessage IWillDestroyYou where
  runMessage msg t@(IWillDestroyYou (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      if not isHero
        then t <$ gainSurge attrs
        else do
          mYotat <- selectOne $ MinionIs Minions.yotatTheDestroyer
          stunned <- maybe (pure True) (\y -> selectAny $ MinionWithId y <> StunnedMinion) mYotat
          for_ mYotat \yotat -> enemyAttacks yotat you
          when stunned $ selectOne ActiveVillain >>= traverse_ (`enemyAttacks` you)
          pure $ IWillDestroyYou (attrs `With` Meta (if stunned then Nothing else (,you) <$> mYotat))
    CheckWindows ws
      | Just (yotat, you) <- boosted meta
      , W.Window W.When (W.EnemyAttack (EnemyMinionId yotat) you) `elem` ws -> do
          changeAttack yotat 1
          pure $ IWillDestroyYou (attrs `With` Meta Nothing)
    -- an attack of his that ended before it was declared gets nothing, nor any after it
    MinionMessage yotat MinionEndAttack
      | Just (yotat', _) <- boosted meta
      , yotat == yotat' ->
          IWillDestroyYou . (`With` Meta Nothing) <$> runMessage msg attrs
    _ -> IWillDestroyYou . (`With` meta) <$> runMessage msg attrs
