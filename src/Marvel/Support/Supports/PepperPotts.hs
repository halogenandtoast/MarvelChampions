module Marvel.Support.Supports.PepperPotts (
  pepperPotts,
  PepperPotts (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

pepperPotts :: SupportCard PepperPotts
pepperPotts = support PepperPotts Cards.pepperPotts

newtype PepperPotts = PepperPotts (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PepperPotts where
  getAbilities (PepperPotts a) =
    [ ability
        a
        1
        Resource
        ( OwnsThis
            <> ExtendedCardExists
              (TopmostCardInDiscardOf (IdentityWithId $ supportController a) AnyCard)
        )
        ExhaustCost
        ( Pay $
            ResourcePaymentFromCard $
              TopmostCardInDiscardOf (IdentityWithId $ supportController a) AnyCard
        )
    ]

instance RunMessage PepperPotts where
  runMessage msg (PepperPotts attrs) = PepperPotts <$> runMessage msg attrs
