module Marvel.Treachery.Treacheries.Nc48038 where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Treachery.Types
import Marvel.Treachery.Cards qualified as Cards

nc48038 :: TreacheryCard Nc48038
nc48038 = treachery (Nc48038 . (`With` ([], False))) Cards.nc48038

newtype Nc48038 = Nc48038 (Attrs Treachery `With` ([MinionId], Bool))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsTreachery Nc48038 where
  toTreacheryAttrs (Nc48038 (attrs `With` _)) = attrs
instance RunMessage Nc48038 where
  runMessage msg a@(Nc48038 (attrs `With` (watching, activated))) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      minions <- selectList AnyMinion
      for_ minions $ \mid -> getMinionEngagedPlayer mid >>= traverse_ (minionActivatesAgainst mid)
      cardSpecific attrs "afterActivations" you
      pure $ Nc48038 (attrs {treacheryResolver = Just you} `With` (minions, False))
    CheckWindows windows -> do
      let completed = any (\case W.Window W.After (W.EnemyActivation (EnemyMinionId mid) _) -> mid `elem` watching; _ -> False) windows
      pure $ Nc48038 (attrs `With` (watching, activated || completed))
    CardSpecific (isTarget attrs -> True) "afterActivations" _ -> do
      unless activated $ discardUntil FromEncounterDeck (CardWithType MinionType) attrs
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      for_ (treacheryResolver attrs) (\you -> revealFoundEncounterCard you card)
      pure a
    _ -> (\updated -> Nc48038 (updated `With` (watching, activated))) <$> runMessage msg attrs
