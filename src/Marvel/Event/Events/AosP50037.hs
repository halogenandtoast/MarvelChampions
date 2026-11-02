{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Event.Events.AosP50037 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Villain.Types qualified
import Marvel.Minion.Types qualified
import Marvel.Stats (unSch)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

aosP50037 :: EventCard AosP50037
aosP50037 = event AosP50037 Cards.aosP50037

newtype AosP50037 = AosP50037 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50037 where
  runMessage msg card@(AosP50037 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        sch <- getEnemyPrintedScheme enemy
        createEffect Cards.aosP50037 attrs (EnemyEntity $ EnemyWithId enemy)
        cardSpecific attrs "concentratedFireSetup" (you,enemy,sch)
        dealDamage enemy attrs (withRanged $ toDamage 4 $ FromPlayerAttack you)
        | enemy <- enemies]
      pure card
    _ -> AosP50037 <$> runMessage msg attrs

newtype ConcentratedFireEffect = ConcentratedFireEffect (Attrs Effect `With` Maybe (IdentityId,EnemyId,Natural))
  deriving newtype (Show,Eq,ToJSON,FromJSON)
instance IsEffect ConcentratedFireEffect where
  toEffectAttrs (ConcentratedFireEffect (attrs `With` _)) = attrs
concentratedFireEffect :: CardEffect ConcentratedFireEffect
concentratedFireEffect = effect (ConcentratedFireEffect . (`With` Nothing)) Cards.aosP50037
instance RunMessage ConcentratedFireEffect where
  runMessage msg e@(ConcentratedFireEffect (attrs `With` stored)) = runQueueT $ case msg of
    CardSpecific target "concentratedFireSetup" (fromJSON -> Success payload) | target == refocus (effectSource attrs) ->
      pure $ ConcentratedFireEffect (attrs `With` Just payload)
    CheckWindows windows | Just (you,enemy,sch) <- stored -> do
      let matches = \case
            W.Window W.After (W.DefeatedMinion mid _ source _) -> enemy == EnemyMinionId mid && source == effectSource attrs
            W.Window W.After (W.DefeatedVillain vid _ source _) -> enemy == EnemyVillainId vid && source == effectSource attrs
            _ -> False
      when (any matches windows) do
        chooseOne you [labeled "$label.addThreat" (placeSuitThreat you sch),labeled "$label.changeSuit" (changeSuit you True)]
        disableEffect (effectId attrs)
      pure e
    EventMessage eid ResolvedEvent | effectSource attrs == EventRef eid -> e <$ disableEffect (effectId attrs)
    _ -> ConcentratedFireEffect . (`With` stored) <$> runMessage msg attrs
