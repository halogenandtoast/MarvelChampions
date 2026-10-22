module Marvel.Support.Supports.HallOfHeroes (
  hallOfHeroes,
  HallOfHeroes (..),
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
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

hallOfHeroes :: SupportCard HallOfHeroes
hallOfHeroes = support HallOfHeroes Cards.hallOfHeroes

-- | Its glory counters are kept as its uses: it is not discarded without them.
newtype HallOfHeroes = HallOfHeroes (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HallOfHeroes where
  getAbilities a@(HallOfHeroes attrs) =
    limitedWindowAbility
      a
      1
      (W.EnemyDefeated W.After MinionEnemy (DamageByPlayer $ IdentityWithId $ supportController attrs))
      Response
      OwnsThis
      NoCost
      (runAbility a 1)
      : [ ability a 2 AlterEgoAction OwnsThis (ExhaustCost <> mconcat (replicate 3 UseCost)) (runAbility a 2)
        | supportUses attrs >= 3
        ]

instance RunMessage HallOfHeroes where
  runMessage msg h@(HallOfHeroes attrs) = case msg of
    RanAbility _ (isTarget h -> True) 1 _ _ -> pure . HallOfHeroes $ attrs & usesL +~ 1
    RanAbility ident (isTarget h -> True) 2 _ _ -> do
      push $ IdentityMessage ident $ DrawCards FromDeck 3
      pure h
    _ -> HallOfHeroes <$> runMessage msg attrs
