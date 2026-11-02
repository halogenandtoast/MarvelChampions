module Marvel.Ally.Allies.ScarletSpiderSilk where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Ally.Runner

scarletSpiderSilk :: AllyCard ScarletSpiderSilk
scarletSpiderSilk = ally ScarletSpiderSilk Cards.scarletSpiderSilk (Thw 1, 2) (Atk 1, 2) (HP 5)
newtype ScarletSpiderSilk = ScarletSpiderSilk (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ScarletSpiderSilk where
  runMessage msg u@(ScarletSpiderSilk attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.IdentityTakeDamage you _ <- windows] $ \you -> do
        pending <- pendingCharacterDamage you
        for_ pending $ \(source,damage) -> do
          preventDamage you (damageAmount damage)
          damageCharacter (allyId attrs) source damage
      for_ [aid | W.AllyTakeDamage aid _ <- windows] $ \aid -> do
        pending <- pendingCharacterDamage aid
        for_ pending $ \(source,damage) -> do
          preventAllyDamage aid (damageAmount damage)
          damageCharacter (allyId attrs) source damage
      pure u
    _ -> ScarletSpiderSilk <$> runMessage msg attrs

instance HasAbilities ScarletSpiderSilk where
  getAbilities u@(ScarletSpiderSilk attrs) = [limitedWindowAbility u 1 (W.WindowOneOf [W.WouldTakeDamage (IdentityWithTrait T.WebWarrior) FromAnyDamageSource AnyValue, W.AllyWouldTakeDamage (AllyWithTrait T.WebWarrior <> NotAlly (AllyWithId $ allyId attrs))]) Interrupt OwnsThis NoCost (runAbility u 1)]
