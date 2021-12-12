module Marvel.Support.Supports.PepperPotts (
  pepperPotts,
  PepperPotts (..),
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
import Marvel.Support.Cards qualified as Cards
import Marvel.Target

pepperPotts :: SupportCard PepperPotts
pepperPotts = support PepperPotts Cards.pepperPotts

newtype PepperPotts = PepperPotts SupportAttrs
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities PepperPotts where
  getAbilities (PepperPotts a) =
    [ ability
        a
        1
        Resource
        ( OwnsThis
            <> ExtendedCardExists
              (TopOfDiscardOf $ IdentityWithId (supportController a))
        )
        ExhaustCost
        ( Pay $
            ResourcePaymentFromCard $
              TopOfDiscardOf $
                IdentityWithId
                  (supportController a)
        )
    ]

instance RunMessage PepperPotts where
  runMessage msg (PepperPotts attrs) = PepperPotts <$> runMessage msg attrs
