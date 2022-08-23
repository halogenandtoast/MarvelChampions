module Marvel.Support.Supports.TacTeam (
  tacTeam,
  TacTeam (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Support.Types
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

tacTeam :: SupportCard TacTeam
tacTeam =
  supportWith TacTeam Cards.tacTeam $ (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype TacTeam = TacTeam SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities TacTeam where
  getAbilities (TacTeam a) =
    [ ability
        a
        1
        Action
        (OwnsThis <> EnemyExists DamageableEnemy)
        (ExhaustCost <> UseCost)
        (ChooseDamage (toSource a) (toDamage 2 FromAbility) DamageableEnemy)
    ]

instance RunMessage TacTeam where
  runMessage msg (TacTeam attrs) = TacTeam <$> runMessage msg attrs
