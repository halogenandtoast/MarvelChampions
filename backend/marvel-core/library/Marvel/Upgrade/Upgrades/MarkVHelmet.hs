module Marvel.Upgrade.Upgrades.MarkVHelmet
  ( markVHelmet
  , MarkVHelmet(..)
  )
where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Trait
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

markVHelmet :: UpgradeCard MarkVHelmet
markVHelmet = upgrade MarkVHelmet Cards.markVHelmet

newtype MarkVHelmet = MarkVHelmet UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities MarkVHelmet where
  getAbilities (MarkVHelmet a) = [ability a 1 HeroAction (OwnsThis <> SchemeExists ThwartableScheme) ExhaustCost $ RunAbility (toTarget a) 1]

instance RunMessage MarkVHelmet where
  runMessage msg u@(MarkVHelmet attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> thwartGuard u $ do
      let ident = upgradeController attrs
      aerial <- selectAny (IdentityWithId ident <> IdentityWithTrait Aerial)
      schemes <- selectList ThwartableScheme
      if aerial
         then
            traverse_ (\sid -> pushChoice ident (ThwartScheme (SchemeTarget sid) (toSource attrs) 1)) schemes
         else
          chooseOne ident $ map (thwartChoice attrs 1) schemes
      pure u
    _ -> MarkVHelmet <$> runMessage msg attrs
