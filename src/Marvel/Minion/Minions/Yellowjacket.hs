module Marvel.Minion.Minions.Yellowjacket (
  yellowjacket,
  Yellowjacket (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Modifier ()
import Marvel.Ref ()
import Marvel.Trait

yellowjacket :: MinionCard Yellowjacket
yellowjacket = minion Yellowjacket Cards.yellowjacket (Sch 2) (Atk 2) (HP 4)

newtype Yellowjacket = Yellowjacket (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | While you are in Giant hero form, Yellowjacket gains the Giant trait and retaliate 1.
-- While you are in Tiny hero form, Yellowjacket gains the Tiny trait and gets +1 ATK.
-- "You" is the player he is engaged with.
instance HasModifiersFor Yellowjacket where
  getModifiersFor _ (MinionRef mid) (Yellowjacket a) | mid == minionId a = do
    let you = minionEngagedIdentity a
    giant <- identityMatches (HeroIdentity <> IdentityWithTrait Giant) you
    tiny <- identityMatches (HeroIdentity <> IdentityWithTrait Tiny) you
    pure $
      [m | giant, m <- [TraitModifier Giant, KeywordModifier (Retaliate 1)]]
        <> [m | tiny, m <- [TraitModifier Tiny, AttackModifier 1]]
  getModifiersFor _ _ _ = pure []

instance RunMessage Yellowjacket where
  runMessage msg (Yellowjacket attrs) = Yellowjacket <$> runMessage msg attrs
