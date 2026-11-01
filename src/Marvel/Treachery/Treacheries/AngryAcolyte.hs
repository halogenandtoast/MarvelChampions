module Marvel.Treachery.Treacheries.AngryAcolyte where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as T
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

angryAcolyte :: TreacheryCard AngryAcolyte
angryAcolyte = treachery (AngryAcolyte . (`With` ([], False))) Cards.angryAcolyte

newtype AngryAcolyte = AngryAcolyte (Attrs Treachery `With` ([MinionId], Bool))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery AngryAcolyte where
  toTreacheryAttrs (AngryAcolyte (attrs `With` _)) = attrs

instance RunMessage AngryAcolyte where
  runMessage msg e@(AngryAcolyte (attrs `With` (watching, activated))) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      acolytes <- selectList (MinionWithTrait T.Acolyte)
      for_ acolytes $ \mid -> getMinionEngagedPlayer mid >>= traverse_ (minionActivatesAgainst mid)
      cardSpecific attrs "afterActivations" you
      pure $ AngryAcolyte (attrs {treacheryResolver = Just you} `With` (acolytes, False))
    CheckWindows windows -> do
      let completed = any (\case W.Window W.After (W.EnemyActivation (EnemyMinionId mid) _) -> mid `elem` watching; _ -> False) windows
      pure $ AngryAcolyte (attrs `With` (watching, activated || completed))
    CardSpecific (isTarget attrs -> True) "afterActivations" _ -> do
      unless activated $ discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait T.Acolyte) attrs
      pure e
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      for_ (treacheryResolver attrs) $ \you -> revealEncounterCard you card
      pure e
    _ -> (\a -> AngryAcolyte (a `With` (watching, activated))) <$> runMessage msg attrs
