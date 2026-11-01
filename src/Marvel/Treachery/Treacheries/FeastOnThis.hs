module Marvel.Treachery.Treacheries.FeastOnThis where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

feastOnThis :: TreacheryCard FeastOnThis
feastOnThis = treachery FeastOnThis Cards.feastOnThis

newtype FeastOnThis = FeastOnThis (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage FeastOnThis where
  runMessage msg t@(FeastOnThis attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      damageIdentity you attrs (toDamage 2 FromTreachery)
      cardSpecific attrs "confuse" you
      pure t
    CardSpecific (isTarget attrs -> True) "confuse" (fromJSON -> Success you) -> do
      already <- identityMatches ConfusedIdentity you
      confuseIdentity you
      when already (gainSurge attrs)
      pure t
    _ -> FeastOnThis <$> runMessage msg attrs
