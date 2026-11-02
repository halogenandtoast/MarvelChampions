{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Obligation.Obligations.JessicaLuke where
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
import Marvel.Obligation.Types

data JLObligation = JLObligation (Attrs Obligation) Natural Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsRef JLObligation where
  toRef (JLObligation a _ _) = toRef a

instance HasCardCode JLObligation where
  toCardCode (JLObligation a _ _) = toCardCode a

instance IsObligation JLObligation where
  toObligationAttrs (JLObligation a _ _) = a
  obligationDisplayCounters (JLObligation _ n _) = n
  obligationDisplayExhausted (JLObligation _ _ exhausted) = exhausted

jl61030, jl61033a, jl61033b, jl61033c, jl62028 :: ObligationCard JLObligation
jl61030 = obligation (\a -> JLObligation a 0 False) Cards.jl61030
jl61033a = obligation (\a -> JLObligation a 3 False) Cards.jl61033a
jl61033b = obligation (\a -> JLObligation a 3 False) Cards.jl61033b
jl61033c = obligation (\a -> JLObligation a 3 False) Cards.jl61033c
jl62028 = obligation (\a -> JLObligation a 0 False) Cards.jl62028

instance HasModifiersFor JLObligation where
  getModifiersFor _ (IdentityRef you) (JLObligation a _ _) =
    pure [AdditionalChangeFormCost (DiscardHandCardsCost 1 $ Just 1) | toCardCode a == "61030", obligationOwner a == Just you]
  getModifiersFor _ _ _ = pure []

instance HasAbilities JLObligation where
  getAbilities (JLObligation a n exhausted) = case obligationOwner a of
    Nothing -> []
    Just owner -> case toCardCode a of
      "61030" -> [ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId owner) NoCost (runAbility a 1)]
      "62028" ->
        [limitedWindowAbility a 1 (W.PhaseEnds W.After PlayerPhase) ForcedResponse (SelfMatches $ IdentityWithId owner) NoCost (runAbility a 1)
        ,limitedWindowAbility a 2 (W.UsesRecovery $ IdentityWithId owner) AlterEgoResponse (SelfMatches $ IdentityWithId owner) NoCost (runAbility a 2)]
      _ -> [ability a 1 ForcedAction (SelfMatches $ IdentityWithId owner) NoCost (runAbility a 1) | n > 0, not exhausted]

instance RunMessage JLObligation where
  runMessage msg e@(JLObligation a n exhausted) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> do
      let target = case toCardCode a of
            "61030" -> IdentityWithTitle "Jessica Jones"
            "62028" -> IdentityWithTitle "Luke Cage"
            _ -> IdentityWithId you
      owner <- fromMaybe you <$> selectOne target
      pure $ JLObligation a {obligationOwner = Just owner} n exhausted
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> case toCardCode a of
      "61030" -> do
        allies <- selectList (AllyControlledBy $ IdentityWithId you)
        confused <- identityMatches ConfusedIdentity you
        chooseOne you $ [targeting aid (removeFromGame aid >> removeFromGame a) | aid <- allies] <>
          [labeled "$label.workLifeConfuse" (confuseIdentity you >> removeFromPlay a) | not confused]
        pure e
      "62028" -> do
        damageIdentity you a (withUnpreventable $ toDamage 3 FromAbility)
        pure e
      _ -> do
        let printed = case toCardCode a of "61033a" -> R.Energy; "61033b" -> R.Mental; _ -> R.Physical
        cards <- selectList (PlayableFromHandOf (IdentityWithId you) $ BasicCardMatches $ CardWithResource printed)
        chooseOne you $ [targeting card (identityStep you $ PlayingCard card Nothing) | card <- cards]
          <> [labeled "$label.suggestionThreat" (placeThreatOnEach a 2 MainScheme)]
        when (n == 1) $ removeFromPlay a
        pure $ JLObligation a (if n == 0 then 0 else n-1) True
    RanAbility _ (isTarget a -> True) 2 _ _ | toCardCode a == "62028" -> e <$ removeFromGame a
    IdentityMessage you ReadyCards | obligationOwner a == Just you -> pure $ JLObligation a n False
    PlaceCounters target amount | target == toRef a -> pure $ JLObligation a (n+amount) exhausted
    RemoveCounters target amount | target == toRef a -> do
      let remaining = if amount >= n then 0 else n-amount
      when (remaining == 0) $ removeFromPlay a
      pure $ JLObligation a remaining exhausted
    _ -> do
      a' <- runMessage msg a
      pure $ JLObligation a' n exhausted
