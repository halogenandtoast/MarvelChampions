module Marvel.Upgrade.Upgrades.Indomitable (
  indomitable,
  Indomitable (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

indomitable :: UpgradeCard Indomitable
indomitable = upgrade Indomitable Cards.indomitable

newtype Indomitable = Indomitable (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Indomitable where
  getAbilities (Indomitable a) =
    [ limitedWindowAbility
        a
        1
        (HeroDefended After (IdentityWithId $ upgradeController a) AnyEnemy)
        Response
        OwnsThis
        NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Indomitable where
  runMessage msg u@(Indomitable attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      pushAll
        [ RemoveFromPlay (toRef u)
        , IdentityMessage ident ReadiedIdentity
        ]
      pure u
    _ -> Indomitable <$> runMessage msg attrs
