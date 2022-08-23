module Marvel.Upgrade.Upgrades.ArcReactor
  ( arcReactor
  , ArcReactor(..)
  )
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards

arcReactor :: UpgradeCard ArcReactor
arcReactor = upgrade ArcReactor Cards.arcReactor

newtype ArcReactor = ArcReactor UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities ArcReactor where
  getAbilities (ArcReactor a) = [ability a 1 HeroAction (OwnsThis <> Exhausted) ExhaustCost ReadyIdentity]

instance RunMessage ArcReactor where
  runMessage msg (ArcReactor attrs) =
    ArcReactor <$> runMessage msg attrs
