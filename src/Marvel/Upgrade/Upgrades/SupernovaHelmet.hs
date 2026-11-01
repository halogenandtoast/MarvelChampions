module Marvel.Upgrade.Upgrades.SupernovaHelmet where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query ()
import Marvel.Question (Choice (Pay))
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait (Trait (Aerial))
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

supernovaHelmet :: UpgradeCard SupernovaHelmet
supernovaHelmet = upgrade SupernovaHelmet Cards.supernovaHelmet

newtype SupernovaHelmet = SupernovaHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SupernovaHelmet where
  getAbilities a = [ability a 1 HeroResource OwnsThis ExhaustCost (Pay $ ResourcePayment Wild)]

instance HasModifiersFor SupernovaHelmet where
  getModifiersFor _ (IdentityRef you) (SupernovaHelmet attrs) | you == upgradeController attrs = do
    nova <- identityMatches (IdentityWithTitle "Nova" <> HeroIdentity) you
    pure [TraitModifier Aerial | nova]
  getModifiersFor _ _ _ = pure []

instance RunMessage SupernovaHelmet where
  runMessage msg (SupernovaHelmet attrs) = SupernovaHelmet <$> runMessage msg attrs
