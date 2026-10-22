module Marvel.Support.Supports.NakiaBahadir (
  nakiaBahadir,
  nakiaBahadirEffect,
  NakiaBahadir (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Supports.Helicarrier (NextCardCostsLessEffect, nextCardCostsLess)
import Marvel.Support.Types

nakiaBahadir :: SupportCard NakiaBahadir
nakiaBahadir = support NakiaBahadir Cards.nakiaBahadir

newtype NakiaBahadir = NakiaBahadir (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities NakiaBahadir where
  getAbilities a@(NakiaBahadir attrs) =
    [ ability a 1 AlterEgoAction OwnsThis ExhaustCost $
        CreateEffect
          Cards.nakiaBahadir
          (toSource a)
          (TargetMatches $ IdentityEntity $ IdentityWithId $ supportController attrs)
    ]

nakiaBahadirEffect :: CardEffect NextCardCostsLessEffect
nakiaBahadirEffect = nextCardCostsLess Cards.nakiaBahadir

instance RunMessage NakiaBahadir where
  runMessage msg (NakiaBahadir attrs) = NakiaBahadir <$> runMessage msg attrs
