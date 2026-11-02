module Marvel.CivilWar.AttackTreacheries where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Attack (Attack (..))
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Entity
import Marvel.EncounterSet qualified as E
import Marvel.Game.Source
import Marvel.Hood.Helpers (controlledCards)
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Types
import Marvel.Window qualified as W

-- Tokens distinguish nested attacks and repeated When Revealed effects.
newtype AttackTreachery = AttackTreachery (Attrs Treachery `With` [(CardId,VillainId,[CharacterId])])
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

attackTreacheryDefinitions :: [CardDef]
attackTreacheryDefinitions = [C.cw56148,C.cw56177]

attackTreacheries :: [TreacheryCard AttackTreachery]
attackTreacheries = map (treachery $ \a -> AttackTreachery (a `With` [])) attackTreacheryDefinitions

instance IsTreachery AttackTreachery where
  toTreacheryAttrs (AttackTreachery (a `With` _)) = a

instance RunMessage AttackTreachery where
  runMessage msg e@(AttackTreachery (a `With` attacks)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      hero <- identityMatches HeroIdentity you
      if not hero then do
        if toCardCode a == "56177" then gainSurge a else
          controlledCards you [AllyType,UpgradeType] >>= chooseAndDiscardControlled you
        pure e
      else do
        leaderOfFamily (if toCardCode a == "56148" then E.CwCaptainAmerica else E.CwSpiderWoman) >>= \case
          Nothing -> pure e
          Just vid -> do
            nonce <- getRandom
            enemyActivationAfterCurrent vid $ enemyAttacksWithCompletionId nonce vid you a
            pure $ AttackTreachery (a `With` ((nonce,vid,[]) : attacks))
    CheckWindows windows | notNull attacks -> do
      updated <- for attacks $ \entry@(nonce,vid,damaged) -> do
        current <- getEnemyAttack $ EnemyVillainId vid
        if (attackCompletionId =<< current) /= Just nonce then pure entry else do
          let targets = [character | W.Window W.After (W.EnemyAttacksAndDamages (EnemyVillainId enemy) character n) <- windows,enemy == vid,n > 0]
          pure (nonce,vid,nub $ damaged <> targets)
      pure $ AttackTreachery (a `With` updated)
    CardSpecific (VillainRef vid) "cwFinesseSpent" (fromJSON -> Success (n :: Natural)) | toCardCode a == "56177" -> do
      current <- getEnemyAttack $ EnemyVillainId vid
      when (any (\(nonce,enemy,_) -> enemy == vid && (attackCompletionId =<< current) == Just nonce) attacks) $
        traverse_ (attackGains vid) ([Ranged | n >= 1] <> [Piercing | n >= 2] <> [Overkill | n >= 3])
      pure e
    CardSpecific (isTarget a -> True) "attackCompletedWithId" (fromJSON -> Success (nonce,details :: Maybe Attack)) -> do
      when (toCardCode a == "56148" && isJust details) $ do
        let damaged = concat [characters | (token,_,characters) <- attacks,token == nonce]
        for_ damaged $ \character -> do
          present <- inAnyArea $ selectAny $ CharacterWithId character
          when present $ giveStatus character StunnedStatus
      pure $ AttackTreachery (a `With` filter (\(token,_,_) -> token /= nonce) attacks)
    _ -> (\updated -> AttackTreachery (updated `With` attacks)) <$> runMessage msg a
