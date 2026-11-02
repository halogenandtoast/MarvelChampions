module Marvel.Support.Supports.TheRaftShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Minion (lookupMinion)
import Marvel.Minion.Types qualified as Minion
import Marvel.Stats (unSch)
import System.Random.Shuffle (shuffleM)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Game.Source
import Marvel.Card

import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Support.Types

theRaftShuri :: SupportCard TheRaftShuri
theRaftShuri = support TheRaftShuri Cards.theRaftShuri
newtype TheRaftShuri = TheRaftShuri (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage TheRaftShuri where
  runMessage msg u@(TheRaftShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      discard <- getEncounterDiscard
      let cards = [card | W.EncounterMinionLeftPlay card <- windows, card `elem` discard]
      for_ cards $ \card -> do
        cardSpecific attrs "tuck" (you,card)
      pure u
    CardSpecific (isTarget attrs -> True) "tuck" (fromJSON -> Success (you,card :: EncounterCard)) -> do
      discard <- getEncounterDiscard
      when (card `elem` discard) $ do
        removeEncounterCardFromPiles card
        cardSpecific attrs "holdEncounterCard" card
        let n = unSch $ Minion.minionScheme $ toAttrs $ lookupMinion (toCardCode card) you (MinionId $ unCardId $ ecCardId card)
        removeThreat you attrs n ThwartableScheme
        cardSpecific attrs "four" you
      pure u
    CardSpecific (isTarget attrs -> True) "four" (fromJSON -> Success (you :: IdentityId)) -> do
      let cards = supportHeldEncounterCards attrs
      when (length cards >= 4) $ do
        random <- shuffleM cards
        for_ (headMay random) $ \card -> do
          players <- getPlayers
          chooseOne you [targeting player (cardSpecific attrs "releaseEncounterCard" card >> identityStep player (DealtEncounterCard card)) | player <- players]
      pure u
    _ -> TheRaftShuri <$> runMessage msg attrs

instance HasAbilities TheRaftShuri where
  getAbilities u@(TheRaftShuri _attrs) = [limitedWindowAbility u 1 W.MinionLeftPlayFromEncounter Response OwnsThis NoCost (runAbility u 1)]
