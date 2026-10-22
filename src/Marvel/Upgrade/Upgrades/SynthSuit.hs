module Marvel.Upgrade.Upgrades.SynthSuit (
  synthSuit,
  SynthSuit (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

synthSuit :: UpgradeCard SynthSuit
synthSuit = upgrade SynthSuit Cards.synthSuit

newtype SynthSuit = SynthSuit (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SynthSuit where
  getModifiersFor _ (IdentityRef ident) (SynthSuit attrs)
    | ident == upgradeController attrs = pure [DefenseModifier 1]
  getModifiersFor _ _ _ = pure []

-- | As errata'd: after you resolve the ability of a Preparation card you control.
instance HasAbilities SynthSuit where
  getAbilities a@(SynthSuit attrs) =
    [ limitedWindowAbility
        a
        1
        (ResolvesAbility After (IdentityWithId $ upgradeController attrs) (CardWithTrait Preparation))
        HeroResponse
        OwnsThis
        ExhaustCost
        (Run [IdentityMessage (upgradeController attrs) ReadiedIdentity])
    ]

instance RunMessage SynthSuit where
  runMessage msg (SynthSuit attrs) = SynthSuit <$> runMessage msg attrs
