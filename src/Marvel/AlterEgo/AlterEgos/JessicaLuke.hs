{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.AlterEgo.AlterEgos.JessicaLuke where
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
import Marvel.AlterEgo.Types
import Marvel.AlterEgo.Runner ()

newtype JLAlterEgo = JLAlterEgo (Attrs AlterEgo)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsAlterEgo

jl61001b :: AlterEgoCard JLAlterEgo
jl61001b = alterEgo JLAlterEgo Cards.jl61001b (HP $ Static 11) (HandSize 6) (Rec 4) [Cards.jl61030]

jl62001b :: AlterEgoCard JLAlterEgo
jl62001b = alterEgo JLAlterEgo Cards.jl62001b (HP $ Static 10) (HandSize 6) (Rec 4) [Cards.jl62028]

instance HasModifiersFor JLAlterEgo where
  getModifiersFor _ (IdentityRef you) (JLAlterEgo a)
    | you == alterEgoIdentityId a, toCardCode a == "62001b" = pure [CanHaveAnyNumberOfToughStatusCards, AttacksAgainstLoseKeyword K.Piercing]
  getModifiersFor _ _ _ = pure []

instance HasAbilities JLAlterEgo where
  getAbilities (JLAlterEgo a) = case toCardCode a of
    "61001b" ->
      [ ability a 1 Setup IsSelf NoCost (runAbility a 1)
      , label "$label.incognitoMode" $ ability a 2 Action (IsSelf <> SelfMatches IdentityCanChangeForm) NoCost (runAbility a 2)
      ]
    "62001b" -> [limited (PerPhase 1) $ ability a 1 Action
      (IsSelf <> AnyCriteria [ExtendedCardExists (InDeckOf self $ BasicCardMatches $ CardIs Cards.jl62008), ExtendedCardExists (InDiscardOf self $ BasicCardMatches $ CardIs Cards.jl62008)]) NoCost (runAbility a 1)]
    _ -> []
   where self = IdentityWithId $ alterEgoIdentityId a

instance RunMessage JLAlterEgo where
  runMessage msg e@(JLAlterEgo a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      case toCardCode a of
        "61001b" -> putCardFromSetAsideIntoPlay you "61002"
        "62001b" -> search you (SearchIdentityDeckAndDiscardPile you) (CardIs Cards.jl62008) SearchDrawOne ShuffleBackIn
        _ -> pure ()
      pure e
    RanAbility you (isTarget a -> True) 2 _ _ -> do
      changeToHeroForm you
      lastingModifiers a (IdentityEntity $ IdentityWithId you) [CannotChangeToAlterEgoForm] False
      pure e
    _ -> JLAlterEgo <$> runMessage msg a
