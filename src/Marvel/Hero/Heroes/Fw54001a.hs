{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Hero.Heroes.Fw54001a where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Hero.Types
import Marvel.Hero.Runner ()

fw54001a :: HeroCard Fw54001a
fw54001a = hero Fw54001a Cards.fw54001a (HP $ Static 11) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype Fw54001a = Fw54001a (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54001a where
  getAbilities a@(Fw54001a attrs) =
    [label "$label.lethalProtector" $ limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy (AttackFromPlayer $ IdentityWithId $ heroIdentityId attrs)) Response (IsSelf <> SchemeExists ThwartableScheme) (NoCost) (runAbility a 1)]

instance RunMessage Fw54001a where
  runMessage msg a@(Fw54001a attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      removeThreat you attrs 2 ThwartableScheme
      pure a
    _ -> Fw54001a <$> runMessage msg attrs
