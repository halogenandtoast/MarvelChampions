module Marvel.Hero.Heroes.Groot (
  groot,
  Groot (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

groot :: HeroCard Groot
groot =
  hero
    Groot
    Cards.groot
    (HP $ Static 10)
    (HandSize 5)
    (Thw 1)
    (Atk 2)
    (Def 3)

-- | Flora Colossus — Forced Interrupt: when Groot would take any amount of damage,
-- remove that many growth counters from him; for each one removed, prevent 1 of that
-- damage. As much as he can (rulebook FAQ: 3 counters against 5 damage prevent 3). A
-- tough status card prevents the damage first, so with one he removes none.
instance HasAbilities Groot where
  getAbilities a@(Groot attrs) =
    [ label "Flora Colossus" $
        limitedWindowAbility
          a
          1
          (W.WouldTakeDamage (IdentityWithId $ heroIdentityId attrs) FromAnyDamageSource (GreaterThan $ Static 0))
          ForcedInterrupt
          (SelfMatches $ IdentityWithCounters $ GreaterThan $ Static 0)
          NoCost
          (runAbility a 1)
    ]

newtype Groot = Groot (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Groot where
  runMessage msg h@(Groot attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      counters <- project PlayerIdentityCounters ident
      tough <- project PlayerIdentityTough ident
      pendingDamage ident >>= traverse_ \damage -> do
        let n = min counters (damageAmount damage)
        unless ((tough && preventedByTough damage) || n == 0) do
          removeCounters ident n
          preventDamage ident n
      pure h
    _ -> Groot <$> runMessage msg attrs
