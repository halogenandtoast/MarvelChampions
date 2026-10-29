module Marvel.SideScheme.SideSchemes.SelfPreservation (selfPreservation, SelfPreservation (..)) where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Ally.Cards qualified as Allies
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

selfPreservation :: SideSchemeCard SelfPreservation
selfPreservation = sideScheme SelfPreservation Cards.selfPreservation (PerPlayer 2)

newtype SelfPreservation = SelfPreservation (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- RR Referential Ability: Nebula's own identity/signature/nemesis cards take
-- precedence over other cards with the same title.
instance HasModifiersFor SelfPreservation where
  getModifiersFor _ ref _ = case ref of
    IdentityRef ident -> do
      nebula <- identityMatches (IdentityWithTitle "Nebula") ident
      pure [modifier | nebula, modifier <- [ThwartPenalty 1, AttackPenalty 1, DefensePenalty 1]]
    AllyRef aid -> do
      gamora <- selectAny $ AllyWithId aid <> AllyIs Allies.gamoraNebula
      pure [modifier | gamora, modifier <- [AttackModifier 1, KeywordModifier Piercing]]
    MinionRef mid -> do
      gamora <- (||) <$> selectAny (MinionWithId mid <> MinionIs Minions.gamoraNemesis) <*> selectAny (MinionWithId mid <> MinionIs Allies.gamoraNebula)
      pure [modifier | gamora, modifier <- [AttackModifier 1, KeywordModifier Piercing]]
    _ -> pure []

instance RunMessage SelfPreservation where
  runMessage msg (SelfPreservation attrs) = runQueueT $ SelfPreservation <$> runMessage msg attrs
