-- | Loki (The Mad Titan's Shadow, scenario 5): five versions of one stage I, one in play
-- and the others set aside (MTS rulebook p.24). What advances or swaps him is the
-- scenario's; each version's keywords (retaliate, stalwart, piercing, victory 1) are its
-- card's.
module Marvel.Villain.Villains.Loki (
  Loki (..),
  loki1,
  loki2,
  loki3,
  loki4,
  loki5,
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype Loki = Loki (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

loki1 :: VillainCard Loki
loki1 = villain Loki Cards.loki1 (Sch 2) (Atk 2) (HP $ PerPlayer 20)

loki2 :: VillainCard Loki
loki2 = villain Loki Cards.loki2 (Sch 2) (Atk 1) (HP $ PerPlayer 20)

loki3 :: VillainCard Loki
loki3 = villain Loki Cards.loki3 (Sch 1) (Atk 3) (HP $ PerPlayer 20)

loki4 :: VillainCard Loki
loki4 = villain Loki Cards.loki4 (Sch 3) (Atk 1) (HP $ PerPlayer 20)

loki5 :: VillainCard Loki
loki5 = villain Loki Cards.loki5 (Sch 1) (Atk 2) (HP $ PerPlayer 20)

-- | 21160: Loki cannot take damage while a side scheme is in play.
instance HasModifiersFor Loki where
  getModifiersFor _ (VillainRef vid) (Loki a)
    | vid == villainId a && cdCardCode (getCardDef a) == cdCardCode Cards.loki1 = do
        schemes <- selectAny AnySideScheme
        pure [CannotTakeDamage | schemes]
  getModifiersFor _ _ _ = pure []

-- | Defeated: first his "when defeated" interrupts (Jormungand's), then the defeat as
-- the scenario resolves it (the victory display, the win, the next Loki), then his When
-- Defeated: discard cards from the top of the encounter deck until a side scheme is
-- discarded, the active player revealing it.
instance RunMessage Loki where
  runMessage msg e@(Loki attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      checkWindows [W.Window W.When $ W.VillainDefeat ident]
      resolveVillainDefeat ident
      you <- getActivePlayerId
      discardUntil FromEncounterDeck (CardWithType SideSchemeType) (IdentityRef you)
      pure e
    _ -> Loki <$> runMessage msg attrs
