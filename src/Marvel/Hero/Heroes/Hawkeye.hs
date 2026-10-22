module Marvel.Hero.Heroes.Hawkeye where

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
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Upgrade.Cards qualified as Upgrades

hawkeye :: HeroCard Hawkeye
hawkeye =
  hero
    Hawkeye
    Cards.hawkeye
    (HP $ Static 9)
    (HandSize 5)
    (Thw 1)
    (Atk 2)
    (Def 1)

-- | "Quick Draw" — Action: Exhaust Hawkeye → ready Hawkeye's Bow.
instance HasAbilities Hawkeye where
  getAbilities (Hawkeye a) =
    [ label "\"Quick Draw\"" $
        ability a 1 Action (IsSelf <> UpgradeExists (UpgradeIs Upgrades.hawkeyesBow <> upgradeControlledBy (heroIdentityId a))) ExhaustCost $
          runAbility a 1
    ]

newtype Hawkeye = Hawkeye (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Hawkeye where
  runMessage msg h@(Hawkeye attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      bows <- selectList $ UpgradeIs Upgrades.hawkeyesBow <> upgradeControlledBy ident
      for_ bows ready
      pure h
    _ -> Hawkeye <$> runMessage msg attrs
