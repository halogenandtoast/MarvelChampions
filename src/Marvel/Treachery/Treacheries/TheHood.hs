module Marvel.Treachery.Treacheries.TheHood where

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Prelude
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Villain.Villains.TheHood

newtype HoodTreachery = HoodTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

fieldRecruitment, upperHand :: TreacheryCard HoodTreachery
fieldRecruitment = treachery HoodTreachery Cards.fieldRecruitment
upperHand = treachery HoodTreachery Cards.upperHand

instance RunMessage HoodTreachery where
  runMessage msg e@(HoodTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      if toCardCode attrs == "24012"
        then do
          recruitModular
          foulPlay you
          removeFromGame attrs
        else do
          villain <- selectOne ActiveVillain
          -- The active Hood may be any stage.
          for_ villain \vid -> villainActivatesAgainst vid you
          foulPlay you
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "24012" -> do
      you <- getBoostedPlayer enemy
      -- Foul Play follows the activation, including all its responses.
      v <- selectJust ActiveVillain
      activationAfter enemy v (foulPlay you)
      pure e
    _ -> HoodTreachery <$> runMessage msg attrs
