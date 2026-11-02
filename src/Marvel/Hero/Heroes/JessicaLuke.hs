{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Hero.Heroes.JessicaLuke where
import Marvel.Prelude
import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap
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
import Marvel.Keyword qualified as K
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
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers
import Marvel.Phase
import Marvel.Hero.Types
import Marvel.Hero.Runner ()

newtype JLHero = JLHero (Attrs Hero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsHero

jl61001a :: HeroCard JLHero
jl61001a = hero JLHero Cards.jl61001a (HP $ Static 11) (HandSize 5) (Thw 3) (Atk 2) (Def 1)

jl62001a :: HeroCard JLHero
jl62001a = hero JLHero Cards.jl62001a (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 2) (Def 1)

instance HasModifiersFor JLHero where
  getModifiersFor _ (IdentityRef you) (JLHero a)
    | you == heroIdentityId a, toCardCode a == "62001a" = pure [CanHaveAnyNumberOfToughStatusCards, AttacksAgainstLoseKeyword K.Piercing]
  getModifiersFor _ _ _ = pure []

instance HasAbilities JLHero where
  getAbilities (JLHero a) = case toCardCode a of
    "61001a" -> [label "$label.gatherEvidence" $ limitedWindowAbility a 1
      (W.WindowOneOf [W.MakesBasicAttack W.After self, W.MakesBasicThwart W.After self, W.MakesBasicDefense W.After self AnyEnemy])
      Response IsSelf NoCost (runAbility a 1)]
    "62001a" -> [limitedWindowAbility a 1 (W.IdentityDiscardsTough W.After self) ForcedResponse IsSelf NoCost (runAbility a 1)]
    _ -> []
   where self = IdentityWithId $ heroIdentityId a

instance RunMessage JLHero where
  runMessage msg e@(JLHero a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      case toCardCode a of
        "61001a" -> addEvidence you 1
        "62001a" -> damageIdentity you a (withUnpreventable $ toDamage 1 FromAbility)
        _ -> pure ()
      pure e
    _ -> JLHero <$> runMessage msg a
