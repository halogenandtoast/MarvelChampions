module Marvel.Treachery.Treacheries.MetacidalTendencies where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Game.Source
import Marvel.Damage
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Trait (Trait (DeadpoolCorps))
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

metacidalTendencies :: TreacheryCard MetacidalTendencies
metacidalTendencies = treachery MetacidalTendencies Cards.metacidalTendencies

newtype MetacidalTendencies = MetacidalTendencies (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MetacidalTendencies where
  runMessage msg t@(MetacidalTendencies attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId attrs -> do
      characters <- selectList (CharacterWithTrait DeadpoolCorps)
      targets <- filterM (fmap not . (\character -> cannotTakeDamageFrom (characterRef character) (toSource attrs))) characters
      dreadpool <- selectAny (MinionIs Minions.dreadpool)
      if null targets then addAccelerationToken else
        damageCharacters attrs (toDamage (if dreadpool then 3 else 2) FromTreachery) targets
      pure t
    _ -> MetacidalTendencies <$> runMessage msg attrs
