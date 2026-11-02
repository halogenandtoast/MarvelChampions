module Marvel.Treachery.Treacheries.TitaniasFury (
  titaniasFury,
  TitaniasFury (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

titaniasFury :: TreacheryCard TitaniasFury
titaniasFury =
  treachery (TitaniasFury . (`With` Meta False)) Cards.titaniasFury

newtype Meta = Meta {titaniaAttacked :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype TitaniasFury = TitaniasFury (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery TitaniasFury where
  toTreacheryAttrs (TitaniasFury (attrs `With` _)) = attrs

instance RunMessage TitaniasFury where
  runMessage msg t@(TitaniasFury (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        -- "your hero": an alter-ego is not attacked
        mTitania <- selectOne $ MinionIs Cards.titania
        isHero <- identityMatches HeroIdentity identityId
        case mTitania of
          Just titaniaId | isHero -> do
            enemyAttacks titaniaId identityId
            pure t
          _ ->
            pure . TitaniasFury . (`With` meta) $ attrs & surgeL .~ True
      ResolvedTreachery _ ->
        if titaniaAttacked meta
          then TitaniasFury . (`With` meta) <$> runMessage msg attrs
          else do
            -- discarded, and its surge had, before Titania heals
            t' <- TitaniasFury . (`With` meta) <$> runMessage msg (attrs & surgeL .~ True)
            traverse_ healAllDamage =<< selectOne (MinionIs Cards.titania)
            pure t'
      _ -> TitaniasFury . (`With` meta) <$> runMessage msg attrs
    MinionMessage minionId MinionAttacked -> do
      isTitania <- minionMatches (MinionIs Cards.titania) minionId
      let meta' = if isTitania then Meta True else meta
      pure $ TitaniasFury (attrs `With` meta')
    Boost msg' -> case msg' of
      RevealedAsBoost target enemyId | isTarget attrs target -> do
        case enemyId of
          EnemyVillainId villainId -> dealBoost villainId
          EnemyMinionId _ -> pure ()
          EnemyAttachmentId _ -> pure ()
        pure t
      _ -> TitaniasFury . (`With` meta) <$> runMessage msg attrs
    _ -> TitaniasFury . (`With` meta) <$> runMessage msg attrs
