module Marvel.Ally.Allies.AyoShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers ()

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Ally.Runner

ayoShuri :: AllyCard AyoShuri
ayoShuri = ally AyoShuri Cards.ayoShuri (Thw 1, 1) (Atk 2, 1) (HP 3)
newtype AyoShuri = AyoShuri (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage AyoShuri where
  runMessage msg u@(AyoShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      allies <- selectList (AllyWithTrait T.DoraMilaje <> NotAlly (AllyWithId $ allyId attrs))
      chooseOne you [targetingAbility aid 2 | aid <- allies]
      pure u
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      chooseDamage you attrs (toDamage 1 FromAbility) AnyEnemy
      pure u
    _ -> AyoShuri <$> runMessage msg attrs

instance HasAbilities AyoShuri where
  getAbilities u@(AyoShuri attrs) = [limitedWindowAbility u 1 (W.WindowOneOf [W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy, W.AllyThwarted W.After (AllyWithId $ allyId attrs) AnyScheme, W.AllyDefendsAgainst W.After (AllyWithId $ allyId attrs) AnyEnemy]) Response (OwnsThis <> AllyExists (AllyWithTrait T.DoraMilaje <> NotAlly (AllyWithId $ allyId attrs))) NoCost (runAbility u 1)]
