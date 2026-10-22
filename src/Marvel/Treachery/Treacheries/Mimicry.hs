module Marvel.Treachery.Treacheries.Mimicry (
  mimicry,
  Mimicry (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

mimicry :: TreacheryCard Mimicry
mimicry = treachery Mimicry Cards.mimicry

newtype Mimicry = Mimicry (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "When Revealed (Alter-Ego): Discard the top 5 cards of your deck. If a Thwart card was
-- discarded this way, Taskmaster schemes. When Revealed (Hero): Discard the top 5 cards
-- of your deck. If an Attack card was discarded this way, Taskmaster attacks you."
instance RunMessage Mimicry where
  runMessage msg t@(Mimicry attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      discardFromDeck identityId 5 attrs
      pure . Mimicry $ attrs & resolverL ?~ identityId
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      for_ (treacheryResolver attrs) \you -> do
        hero <- identityMatches HeroIdentity you
        villain <- selectJust ActiveVillain
        let discarded trait = any (cardMatch (CardWithTrait trait)) cards
        if hero
          then when (discarded Attack) $ enemyAttacks villain you
          else when (discarded Thwart) $ villainSchemes villain
      pure t
    _ -> Mimicry <$> runMessage msg attrs
