module Marvel.Treachery.Treacheries.AgeOfApocalypseSavageLand where

import Marvel.Prelude
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.AgeOfApocalypse.Settings
import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

import Marvel.Domino (discardedResourceCount)
import Marvel.Environment.Cards qualified as Environments
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype AoaSavageLandTreachery = AoaSavageLandTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

landOutOfTime :: TreacheryCard AoaSavageLandTreachery
landOutOfTime = treachery AoaSavageLandTreachery Cards.landOutOfTime

instance RunMessage AoaSavageLandTreachery where
  runMessage msg e@(AoaSavageLandTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      savage <- selectAny (EnvironmentIs Environments.theSavageLand)
      if savage then resolveSettingSpecialFor you attrs
        else searchEverywhereAndRevealEncounterCard you (CardIs Environments.theSavageLand)
      pure $ AoaSavageLandTreachery (attrs & resolverL ?~ you)
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      for_ (treacheryResolver attrs) \you -> do
        amount <- discardedResourceCount you cards
        dealIndirectDamage you attrs amount
      pure e
    _ -> AoaSavageLandTreachery <$> runMessage msg attrs
