module Marvel.CivilWar.FreedomFighters where

import Marvel.Prelude
import Marvel.Boost (BoostIcon(Star))
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.Entity
import Marvel.Id
import Marvel.Query
import Marvel.Ref
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Treachery.Types

newtype FreedomFighters = FreedomFighters (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

freedomFighters :: [TreacheryCard FreedomFighters]
freedomFighters = map (treachery FreedomFighters) [C.cw56153,C.cw56157]

boostMinionsFor :: HasGame m => IdentityId -> MinionMatcher -> m [MinionId]
boostMinionsFor you matcher = do
  candidates <- asYou you $ selectList matcher
  filterM (\mid -> do
    blanked <- isBlanked mid
    def <- getsGame (`abilityCardDef` MinionRef mid)
    pure $ not blanked && maybe False (elem Star . cdBoostIcons) def) candidates

instance RunMessage FreedomFighters where
  runMessage msg e@(FreedomFighters a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      minions <- boostMinionsFor you AnyMinion
      if null minions then gainSurge a else cardSpecific a "resolveBoosts" (you,Nothing :: Maybe EnemyId,minions)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      minions <- boostMinionsFor you (MinionEngagedWith $ IdentityWithId you)
      unless (null minions) $ cardSpecific a "resolveBoosts" (you,Just enemy,minions)
      pure e
    CardSpecific (isTarget a -> True) "resolveBoosts" (fromJSON -> Success (you,enemy,remaining :: [MinionId])) -> do
      available <- filter (`elem` remaining) <$> boostMinionsFor you AnyMinion
      unless (null available) $ chooseOrRunOne you
        [targeting (MinionRef mid) (do
          resolveInPlayMinionBoost you mid enemy
          cardSpecific a "resolveBoosts" (you,enemy,filter (/= mid) available)) | mid <- available]
      pure e
    Boost inner@(CardSpecific target "resolveBoosts" _) | isTarget a target -> runMessage inner e
    _ -> FreedomFighters <$> runMessage msg a
