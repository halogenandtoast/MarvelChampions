module Marvel.FearNoEvil.KingpinTreacheries where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Modifier
import Marvel.Card
import Marvel.EncounterSet (getNemesisSet)
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.FearNoEvil.Kingpin (revealKingpinNemesis)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Types
import System.Random.Shuffle (shuffleM)

newtype KingpinTreachery = KingpinTreachery (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsTreachery,HasAbilities,HasModifiersFor)
kingpinTreacheries :: [TreacheryCard KingpinTreachery]
kingpinTreacheries = map (treachery KingpinTreachery) [Cards.street60175,Cards.street60176]
instance RunMessage KingpinTreachery where
  runMessage msg e@(KingpinTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      if toCardCode a == "60175" then do
        cards <- shuffleM =<< selectList (NemesisSetFor you)
        case headMay cards of
          Nothing -> gainSurge a
          Just card -> revealFoundEncounterCard you card
      else do
        code <- getIdentityCode you
        cards <- filter (Minions.isNemesisMinion . getCardDef) <$> findEncounterCards (CardInEncounterSet $ getNemesisSet code)
        live <- filterM (selectAny . MinionWithId . MinionId . unCardId . ecCardId) cards
        if null live then revealKingpinNemesis you False else for_ live $ \card -> minionActivatesAgainst (MinionId $ unCardId $ ecCardId card) you
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      if toCardCode a == "60175" then do
        you <- getBoostedPlayer enemy
        cards <- shuffleM =<< selectList (NemesisSetFor you)
        for_ (headMay cards) $ \card -> giveCardAsBoost card enemy
      else whenM (isJust <$> getEnemyAttack enemy) $ attackGains enemy Overkill
      pure e
    _ -> KingpinTreachery <$> runMessage msg a
