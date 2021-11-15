{-# LANGUAGE UndecidableInstances #-}
module Marvel.Support.Supports.AuntMay where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Support.Attrs
import qualified Marvel.Support.Cards as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Question
import Marvel.Target

auntMay :: SupportCard AuntMay
auntMay = support AuntMay Cards.auntMay

instance HasAbilities AuntMay where
  getAbilities a = [ability a 1 AlterEgoAction NoCriteria ExhaustCost $ Heal 4]

newtype AuntMay = AuntMay SupportAttrs
  deriving anyclass IsSupport
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage AuntMay where
  runMessage msg (AuntMay a) = AuntMay <$> runMessage msg a
