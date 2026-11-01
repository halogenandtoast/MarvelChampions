module Marvel.Treachery.Treacheries.PowerAndDecadence where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers (VillainMatcher(ActiveVillain))
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

powerAndDecadence :: TreacheryCard PowerAndDecadence
powerAndDecadence = treachery (PowerAndDecadence . (`With` False)) Cards.powerAndDecadence

newtype PowerAndDecadence = PowerAndDecadence (Attrs Treachery `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery PowerAndDecadence where
  toTreacheryAttrs (PowerAndDecadence (attrs `With` _)) = attrs

instance RunMessage PowerAndDecadence where
  runMessage msg e@(PowerAndDecadence (attrs `With` movedToBoost)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId attrs -> do
      villain <- selectOne ActiveVillain
      for_ villain $ \vid -> do
        enemyBecomesTough vid
        giveCardAsBoost attrs (EnemyVillainId vid)
      pure $ PowerAndDecadence (attrs `With` isJust villain)
    -- It resolves as a treachery, but its new physical zone is the villain's
    -- facedown boost cards, so the ordinary resolved-treachery discard is skipped.
    TreacheryMessage tid (ResolvedTreachery you) | tid == treacheryId attrs, movedToBoost -> do
      removeFromGame attrs
      let card = MkEncounterCard (CardId $ unTreacheryId tid) (getCardDef attrs)
      checkWindows [W.Window W.After $ W.ResolvedTreacheryCard you card]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      enemyActivationAfterCurrent enemy $ enemyActivatesWithoutBoost attrs enemy you
      pure e
    _ -> (\a -> PowerAndDecadence (a `With` movedToBoost)) <$> runMessage msg attrs
