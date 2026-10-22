module Marvel.Support.Supports.BannersLaboratory (
  bannersLaboratory,
  BannersLaboratory (..),
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
import Marvel.Ref
import Marvel.Resource
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

bannersLaboratory :: SupportCard BannersLaboratory
bannersLaboratory = support BannersLaboratory Cards.bannersLaboratory

newtype BannersLaboratory = BannersLaboratory (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Bruce Banner gets +2 REC."
instance HasModifiersFor BannersLaboratory where
  getModifiersFor _ (IdentityRef ident) (BannersLaboratory a)
    | ident == supportController a = do
        banner <- identityMatches (IdentityWithTitle "Bruce Banner") ident
        pure [RecoveryModifier 2 | banner]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BannersLaboratory where
  getAbilities (BannersLaboratory a) =
    [ability a 1 AlterEgoResource OwnsThis ExhaustCost (Pay $ ResourcePayment Mental)]

instance RunMessage BannersLaboratory where
  runMessage msg (BannersLaboratory attrs) =
    BannersLaboratory <$> runMessage msg attrs
