module Marvel.Treachery.Treacheries.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Window qualified as W
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC

wm58028 :: TreacheryCard WmHcTreachery
wm58028 = treachery (WmHcTreachery . (`With` ([], False))) WM.wm58028

wm58029 :: TreacheryCard WmHcTreachery
wm58029 = treachery (WmHcTreachery . (`With` ([], False))) WM.wm58029

hc59040 :: TreacheryCard WmHcTreachery
hc59040 = treachery (WmHcTreachery . (`With` ([], False))) HC.hc59040

newtype WmHcTreachery = WmHcTreachery (Attrs Treachery `With` ([MinionId], Bool))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery WmHcTreachery where
  toTreacheryAttrs (WmHcTreachery (attrs `With` _)) = attrs

instance RunMessage WmHcTreachery where
  runMessage msg t@(WmHcTreachery (attrs `With` (watching, activated))) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      case toCardCode attrs of
        "58028" -> do
          reaper <- selectOne $ MinionIs WM.wm58026
          maybe (dealIndirectDamage you attrs 2) (\mid -> minionActivatesAgainst mid you) reaper
          pure t
        "58029" -> do
          reaper <- selectOne $ MinionIs WM.wm58026
          maybe (searchEverywhereAndRevealEncounterCard you $ CardIs WM.wm58026) (\mid -> minionActivatesAgainst mid you) reaper
          pure t
        _ -> do
          minions <- selectList AnyMinion
          for_ minions $ \mid -> getMinionEngagedPlayer mid >>= traverse_ (enemyAttacks mid)
          cardSpecific attrs "afterGodOfWar" you
          pure $ WmHcTreachery (attrs {treacheryResolver = Just you} `With` (minions, False))
    CheckWindows windows -> do
      let completed = any (\case W.Window W.After (W.EnemyActivation (EnemyMinionId mid) _) -> mid `elem` watching; _ -> False) windows
      pure $ WmHcTreachery (attrs `With` (watching, activated || completed))
    CardSpecific (isTarget attrs -> True) "afterGodOfWar" _ -> do
      unless activated $ discardUntil FromEncounterDeck (CardWithType MinionType) attrs
      pure t
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      for_ (treacheryResolver attrs) $ \you -> revealEncounterCard you card
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target && toCardCode attrs == "58029" -> do
      you <- getBoostedPlayer enemy
      reaper <- selectOne $ MinionIs WM.wm58026
      for_ reaper $ \mid -> enemyActivationAfterCurrent enemy $ minionActivatesAgainst mid you
      pure t
    _ -> (\attrs' -> WmHcTreachery (attrs' `With` (watching, activated))) <$> runMessage msg attrs
