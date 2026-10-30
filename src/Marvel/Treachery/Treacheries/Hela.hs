-- | The Hela set's treacheries (The Mad Titan's Shadow, scenario 4): "Hela" is the
-- villain.
module Marvel.Treachery.Treacheries.Hela (
  helasDomain,
  HelasDomain (..),
  theQueenOfHel,
  TheQueenOfHel (..),
  theWastesOfNiffleheim,
  TheWastesOfNiffleheim (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

helasDomain :: TreacheryCard HelasDomain
helasDomain = treachery (HelasDomain . (`With` DomainMeta False)) Cards.helasDomain

-- | Whether its boost has placed its threat, for the attack.
data DomainMeta = DomainMeta {threatPlaced :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype HelasDomain = HelasDomain (Attrs Treachery `With` DomainMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery HelasDomain where
  toTreacheryAttrs (HelasDomain (attrs `With` _)) = attrs

-- | When Revealed: place 1 threat on the main scheme, and 1 more for each side scheme in
-- the victory display. [star] Boost: if damage from this attack defeats an ally, place 2
-- threat on the main scheme (once for the attack).
instance RunMessage HelasDomain where
  runMessage msg t@(HelasDomain (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      n <- sideSchemesInVictoryDisplay
      placeThreatOnEach attrs (1 + n) EncounterMainScheme
      pure t
    Boost (CheckWindows ws)
      | not (threatPlaced meta)
      , notNull [() | W.Window W.When (W.DefeatedAlly _ (Just _)) <- ws] -> do
          placeThreatOnEach attrs 2 EncounterMainScheme
          pure $ HelasDomain (attrs `With` meta {threatPlaced = True})
    _ -> HelasDomain . (`With` meta) <$> runMessage msg attrs

theQueenOfHel :: TreacheryCard TheQueenOfHel
theQueenOfHel = treachery TheQueenOfHel Cards.theQueenOfHel

newtype TheQueenOfHel = TheQueenOfHel (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): Hela schemes. When Revealed (Hero): Hela attacks you.
-- Either way, then, place 1 threat on each side scheme.
instance RunMessage TheQueenOfHel where
  runMessage msg t@(TheQueenOfHel attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      selectOne ActiveVillain >>= traverse_ \v -> do
        hero <- identityMatches HeroIdentity you
        if hero then enemyAttacks v you else villainSchemes v
      cardSpecific attrs "sideSchemes" ()
      pure t
    CardSpecific (isTarget attrs -> True) "sideSchemes" _ -> do
      traverse_ (`placeThreat` 1) =<< selectList AnySideScheme
      pure t
    _ -> TheQueenOfHel <$> runMessage msg attrs

theWastesOfNiffleheim :: TreacheryCard TheWastesOfNiffleheim
theWastesOfNiffleheim = treachery TheWastesOfNiffleheim Cards.theWastesOfNiffleheim

newtype TheWastesOfNiffleheim = TheWastesOfNiffleheim (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: take 1 indirect damage, and 1 more for each side scheme in the
-- victory display. [star] Boost: this card gains a boost icon for each side scheme in
-- the victory display, as its icons are counted.
instance RunMessage TheWastesOfNiffleheim where
  runMessage msg t@(TheWastesOfNiffleheim attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      n <- sideSchemesInVictoryDisplay
      dealIndirectDamage you attrs (1 + n)
      pure t
    Boost (CheckWindows ws)
      | card : _ <- [c | W.Window W.When (W.CountingBoostIcons c) <- ws, ecCardId c == CardId (unTreacheryId $ treacheryId attrs)] -> do
          n <- sideSchemesInVictoryDisplay
          changeBoostIconCount card (+ n)
          pure t
    _ -> TheWastesOfNiffleheim <$> runMessage msg attrs
