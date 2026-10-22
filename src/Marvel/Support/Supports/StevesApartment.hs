module Marvel.Support.Supports.StevesApartment (
  stevesApartment,
  StevesApartment (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

stevesApartment :: SupportCard StevesApartment
stevesApartment = support StevesApartment Cards.stevesApartment

newtype StevesApartment = StevesApartment (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities StevesApartment where
  getAbilities (StevesApartment a) =
    [ ability a 1 AlterEgoAction OwnsThis ExhaustCost $
        Run
          [ IdentityMessage (supportController a) $ DrawCards FromDeck 1
          , IdentityMessage (supportController a) $ IdentityHealed 1
          ]
    ]

instance RunMessage StevesApartment where
  runMessage msg (StevesApartment attrs) = StevesApartment <$> runMessage msg attrs
