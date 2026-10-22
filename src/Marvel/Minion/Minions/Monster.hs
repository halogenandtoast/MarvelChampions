module Marvel.Minion.Minions.Monster (
  monster,
  Monster (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Card
import Marvel.Damage
import Marvel.Matchers

monster :: MinionCard Monster
monster = minion Monster Cards.monster (Sch 1) (Atk 3) (HP 6)

newtype Monster = Monster (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Monster where
  runMessage msg e@(Monster attrs) = runQueueT $ case msg of
    -- When Revealed: you are stunned; already stunned, you take 2 damage instead
    MinionMessage ident (RevealMinion you) | ident == minionId attrs -> do
      stunned <- identityMatches StunnedIdentity you
      if stunned then damageIdentity you attrs (toDamage 2 FromAbility) else stunIdentity you
      pure e
    -- Boost: after this activation ends, shuffle this card into the encounter deck
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      afterMatching (== ClearBoosts) $ shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unMinionId $ minionId attrs) (getCardDef attrs)]
      pure e
    _ -> Monster <$> runMessage msg attrs
