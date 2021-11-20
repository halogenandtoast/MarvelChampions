module Marvel.Support.Supports.AuntMay where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.Support.Attrs
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

auntMay :: SupportCard AuntMay
auntMay = support AuntMay Cards.auntMay

instance HasAbilities AuntMay where
  getAbilities (AuntMay a) =
    [ ability
          a
          1
          AlterEgoAction
          (OwnsThis <> SelfMatches IdentityWithAnyDamage)
          ExhaustCost
        $ Heal (IdentityCharacter $ supportController a) 4
    ]

newtype AuntMay = AuntMay SupportAttrs
  deriving anyclass IsSupport
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage AuntMay where
  runMessage msg (AuntMay a) = AuntMay <$> runMessage msg a
