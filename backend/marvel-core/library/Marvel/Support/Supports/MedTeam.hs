module Marvel.Support.Supports.MedTeam
  ( medTeam
  , MedTeam(..)
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
import Marvel.Source
import Marvel.Support.Attrs
import qualified Marvel.Support.Cards as Cards
import Marvel.Target

medTeam :: SupportCard MedTeam
medTeam =
  supportWith MedTeam Cards.medTeam $ (usesL .~ 3) . (discardIfNoUsesL .~ True)

newtype MedTeam = MedTeam SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities MedTeam where
  getAbilities (MedTeam a) =
    [ ability
        a
        1
        Action
        (OwnsThis <> CharacterExists CharacterWithAnyDamage)
        (ExhaustCost <> UseCost)
        (ChooseHeal 2 CharacterWithAnyDamage)
    ]

instance RunMessage MedTeam where
  runMessage msg (MedTeam attrs) = MedTeam <$> runMessage msg attrs
