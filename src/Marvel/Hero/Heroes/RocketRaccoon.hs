module Marvel.Hero.Heroes.RocketRaccoon (
  rocketRaccoon,
  RocketRaccoon (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

rocketRaccoon :: HeroCard RocketRaccoon
rocketRaccoon =
  hero
    RocketRaccoon
    Cards.rocketRaccoon
    (HP $ Static 9)
    (HandSize 5)
    (Thw 2)
    (Atk 1)
    (Def 1)

-- | "Murdered You!" — Response: After you deal excess damage to an enemy, draw 1 card.
-- Any damage you or a card you control deals, attack or not; not an ally's.
instance HasAbilities RocketRaccoon where
  getAbilities (RocketRaccoon a) =
    [ label "\"Murdered You!\"" $
        limitedWindowAbility
          a
          1
          (W.DealsExcessDamage (IdentityWithId $ heroIdentityId a))
          Response
          IsSelf
          NoCost
          (runAbility a 1)
    ]

newtype RocketRaccoon = RocketRaccoon (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage RocketRaccoon where
  runMessage msg h@(RocketRaccoon attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      drawCards ident 1
      pure h
    _ -> RocketRaccoon <$> runMessage msg attrs
