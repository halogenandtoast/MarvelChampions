module Marvel.Hero.Heroes.IronMan where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait

ironMan :: HeroCard IronMan
ironMan =
  hero
    IronMan
    Cards.ironMan
    (HP $ Static 9)
    (HandSize 1)
    (Thw 2)
    (Atk 1)
    (Def 1)

instance HasAbilities IronMan where
  getAbilities _ = []

newtype IronMan = IronMan (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasModifiersFor IronMan where
  getModifiersFor _ target (IronMan a) | isTarget a target = do
    x <-
      selectListCount $
        UpgradeControlledBy (IdentityWithId $ heroIdentityId a)
          <> UpgradeWithTrait Tech
    pure [HandSizeModifier x]
  getModifiersFor _ _ _ = pure []

instance RunMessage IronMan where
  runMessage msg (IronMan attrs) = IronMan <$> runMessage msg attrs
