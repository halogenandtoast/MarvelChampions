module Marvel.Hero.Heroes.X23 where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

x23 :: HeroCard X23
x23 = hero X23 Cards.x23 (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 1) (Def 2)

newtype X23 = X23 (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities X23 where
  getAbilities a@(X23 attrs) =
    [ label "$label.livingWeapon" $ limited (PerPhase 1) $
        limitedWindowAbility a 1 (W.IdentityDamaged W.After $ IdentityWithId $ heroIdentityId attrs)
          Response (IsSelf <> CharacterExists (IdentityCharacters (IdentityWithId $ heroIdentityId attrs) <> CharacterCanBeReadied)) NoCost (runAbility a 1)
    ]

instance RunMessage X23 where
  runMessage msg h@(X23 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> h <$ ready (heroIdentityId attrs)
    _ -> X23 <$> runMessage msg attrs
