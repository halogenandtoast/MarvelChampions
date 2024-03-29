module Marvel.Upgrade.Upgrades.MarkVHelmet (
  markVHelmet,
  MarkVHelmet (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

markVHelmet :: UpgradeCard MarkVHelmet
markVHelmet = upgrade MarkVHelmet Cards.markVHelmet

newtype MarkVHelmet = MarkVHelmet (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MarkVHelmet where
  getAbilities (MarkVHelmet a) =
    [ ability
        a
        1
        HeroAction
        (OwnsThis <> SchemeExists ThwartableScheme)
        ExhaustCost
        $ RunAbility (toRef a) 1
    ]

instance RunMessage MarkVHelmet where
  runMessage msg u@(MarkVHelmet attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> thwartGuard u $ do
      aerial <- selectAny (IdentityWithId ident <> IdentityWithTrait Aerial)
      schemes <- selectList ThwartableScheme
      if aerial
        then
          traverse_
            ( \sid ->
                pushChoice
                  ident
                  (ThwartScheme (toRef sid) (toSource attrs) 1)
            )
            schemes
        else chooseOne ident $ map (thwartChoice attrs 1) schemes
      pure u
    _ -> MarkVHelmet <$> runMessage msg attrs
