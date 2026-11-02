{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Treachery.Treacheries.AosP50063 where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Trait qualified as Trait
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

aosP50063 :: TreacheryCard AosP50063
aosP50063 = treachery AosP50063 Cards.aosP50063

newtype AosP50063 = AosP50063 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50063 where
  runMessage msg e@(AosP50063 a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      cards <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
      let matcher = CardWithType MinionType <> CardWithTrait Trait.Leviathan
      if any (cardMatch matcher) cards then searchAndPutIntoPlayEncounterCard you matcher
      else damageIdentity you a $ toDamage 2 FromAbility
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      damageIdentity you a $ toDamage 1 FromAbility
      pure e
    _ -> AosP50063 <$> runMessage msg a
