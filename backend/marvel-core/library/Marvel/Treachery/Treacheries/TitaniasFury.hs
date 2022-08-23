module Marvel.Treachery.Treacheries.TitaniasFury
  ( titaniasFury
  , TitaniasFury(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Types
import Marvel.Treachery.Cards qualified as Cards

titaniasFury :: TreacheryCard TitaniasFury
titaniasFury =
  treachery (TitaniasFury . (`With` Meta False)) Cards.titaniasFury

newtype Meta = Meta { titaniaAttacked :: Bool }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype TitaniasFury = TitaniasFury (TreacheryAttrs `With` Meta)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage TitaniasFury where
  runMessage msg t@(TitaniasFury (attrs `With` meta)) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery ident -> do
          mTitania <- selectOne $ MinionIs Cards.titania
          case mTitania of
            Just titaniaId -> do
              push (MinionMessage titaniaId $ MinionAttacks ident)
              pure t
            Nothing ->
              pure . TitaniasFury . (`With` meta) $ attrs & surgeL .~ True
        ResolvedTreachery _ -> if titaniaAttacked meta
          then TitaniasFury . (`With` meta) <$> runMessage msg attrs
          else do
            mTitania <- selectOne $ MinionIs Cards.titania
            for_ mTitania
              $ \titania -> push (MinionMessage titania MinionHealAllDamage)
            TitaniasFury . (`With` meta) <$> runMessage
              msg
              (attrs & surgeL .~ True)
        _ -> TitaniasFury . (`With` meta) <$> runMessage msg attrs
    MinionMessage minionId MinionAttacked -> do
      isTitania <- minionMatches (MinionIs Cards.titania) minionId
      let meta' = if isTitania then Meta True else meta
      pure $ TitaniasFury (attrs `With` meta')
    Boost msg' -> case msg' of
      RevealedAsBoost target enemyId | isTarget attrs target -> do
        case enemyId of
          EnemyMinionId minionId -> push (DealBoost $ MinionTarget minionId)
          EnemyVillainId villainId ->
            push (DealBoost $ VillainTarget villainId)
        pure t
      _ -> TitaniasFury . (`With` meta) <$> runMessage msg attrs
    _ -> TitaniasFury . (`With` meta) <$> runMessage msg attrs
