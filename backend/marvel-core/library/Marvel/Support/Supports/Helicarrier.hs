{-# LANGUAGE UndecidableInstances #-}
module Marvel.Support.Supports.Helicarrier where

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

helicarrier :: SupportCard Helicarrier
helicarrier = support Helicarrier Cards.helicarrier

instance HasAbilities Helicarrier where
  getAbilities a = [ability a 1 AlterEgoAction NoCriteria ExhaustCost $ Heal 4]

newtype Helicarrier = Helicarrier SupportAttrs
  deriving anyclass IsSupport
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Helicarrier where
  runMessage msg (Helicarrier a) = Helicarrier <$> runMessage msg a
