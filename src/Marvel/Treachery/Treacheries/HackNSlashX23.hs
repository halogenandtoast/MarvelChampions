module Marvel.Treachery.Treacheries.HackNSlashX23 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Hand
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

hackNSlashX23 :: TreacheryCard HackNSlashX23
hackNSlashX23 = treachery HackNSlashX23 Cards.hackNSlashX23

newtype HackNSlashX23 = HackNSlashX23 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HackNSlashX23 where
  runMessage msg e@(HackNSlashX23 attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      discardFor you attrs RandomFromHand 1 1
      pure $ HackNSlashX23 (attrs & resolverL ?~ you)
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      let n = fromIntegral $ sum [length [() | (PrintedResource, _) <- cdResources $ getCardDef card] | card <- cards]
      for_ (treacheryResolver attrs) (\you -> when (n > 0) $ damageIdentity you attrs $ toDamage n FromTreachery)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      revealBoost attrs you
      pure e
    _ -> HackNSlashX23 <$> runMessage msg attrs
