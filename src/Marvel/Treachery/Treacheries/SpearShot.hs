module Marvel.Treachery.Treacheries.SpearShot where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

spearShot :: TreacheryCard SpearShot
spearShot = treachery SpearShot Cards.spearShot

newtype SpearShot = SpearShot (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SpearShot where
  runMessage msg a@(SpearShot attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      discardFromDeckUntil you (CardWithType EventType) attrs
      pure $ SpearShot attrs {treacheryResolver = Just you}
    WithDiscardedMatch (isTarget attrs -> True) FromDeck (PlayerCard card) -> do
      let cost = fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card
      for_ (treacheryResolver attrs) $ \you -> dealIndirectDamage you attrs cost
      when (Trait.Aerial `member` cdTraits (getCardDef card)) $ gainSurge attrs
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      dealIndirectDamage you attrs 2
      pure a
    _ -> SpearShot <$> runMessage msg attrs
