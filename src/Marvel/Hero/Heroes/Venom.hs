module Marvel.Hero.Heroes.Venom (
  venom,
  Venom (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats

venom :: HeroCard Venom
venom =
  hero
    Venom
    Cards.venom
    (HP $ Static 12)
    (HandSize 5)
    (Thw 1)
    (Atk 2)
    (Def 2)

newtype Venom = Venom (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | "You can control 1 additional upgrade that has the restricted keyword."
instance HasModifiersFor Venom where
  getModifiersFor _ target (Venom a) | isTarget a target = pure [RestrictedLimitModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Symbiotic Bond — Resource: take 1 damage → generate a [wild] resource. (Limit once
-- per phase.) The damage is a cost: a tough status card keeps it from being paid.
instance HasAbilities Venom where
  getAbilities a =
    [ label "Symbiotic Bond" $
        limitedAbility a 1 (PerPhase 1) HeroResource IsSelf (DamageCost 1) (Pay $ ResourcePayment Wild)
    ]

instance RunMessage Venom where
  runMessage msg (Venom attrs) = Venom <$> runMessage msg attrs
