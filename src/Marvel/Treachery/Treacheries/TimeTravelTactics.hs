module Marvel.Treachery.Treacheries.TimeTravelTactics (
  timeTravelTactics,
  TimeTravelTactics (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Ref ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

timeTravelTactics :: TreacheryCard TimeTravelTactics
timeTravelTactics = treachery TimeTravelTactics Cards.timeTravelTactics

newtype TimeTravelTactics = TimeTravelTactics (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Surge. When Revealed: each player takes 1 indirect damage for each obligation in
-- their play area. [star] Boost: this card gains a boost icon for each obligation in your
-- play area.
instance RunMessage TimeTravelTactics where
  runMessage msg t@(TimeTravelTactics attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      players <- getPlayers
      for_ players \p -> do
        n <- countObligationsOf p
        when (n > 0) $ dealIndirectDamage p attrs n
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      n <- countObligationsOf =<< getBoostedPlayer enemy
      when (n > 0) case enemy of
        EnemyVillainId vid -> addBoost vid n
        EnemyMinionId mid -> minionStep mid (MinionAddBoost n)
        EnemyAttachmentId _ -> pure ()
      pure t
    _ -> TimeTravelTactics <$> runMessage msg attrs
