module Marvel.Event.Events.PsychicKicker where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Projection
import Marvel.Ally.Types qualified as Ally
import Marvel.Effect.Types

psychicKicker :: EventCard PsychicKicker
psychicKicker = event PsychicKicker Cards.psychicKicker

newtype PsychicKicker = PsychicKicker (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsychicKicker where
  runMessage msg e@(PsychicKicker attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      allies <- selectList AnyAlly
      unless (null allies) $ chooseOne you
        [targeting aid (ready aid >> createEffect Cards.psychicKicker attrs (AllyEntity $ AllyWithId aid)) | aid <- allies]
      pure e
    _ -> PsychicKicker <$> runMessage msg attrs

newtype PsychicKickerEffect = PsychicKickerEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

psychicKickerEffect :: CardEffect PsychicKickerEffect
psychicKickerEffect = effectWith PsychicKickerEffect Cards.psychicKicker $
  (modifiersL .~ [ThwartModifier 2, AttackModifier 2]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage PsychicKickerEffect where
  runMessage msg e@(PsychicKickerEffect attrs) = runQueueT $ case msg of
    AllyMessage aid action | effectMatcher attrs == AllyEntity (AllyWithId aid) -> do
      case action of
        AllyAttackLands enemy _ -> whenM (not <$> selectAny (EnemyWithId enemy)) $ disableEffect (effectId attrs)
        AllyAttackConsequential -> disableEffect (effectId attrs)
        AllyThwartConsequential -> disableEffect (effectId attrs)
        AllyAttacked -> whenM (characterMatches (CharacterWithStatus StunnedStatus) $ AllyCharacter aid) $ disableEffect (effectId attrs)
        AllyThwarted -> whenM (characterMatches (CharacterWithStatus ConfusedStatus) $ AllyCharacter aid) $ disableEffect (effectId attrs)
        _ -> pure ()
      pure e
    _ -> PsychicKickerEffect <$> runMessage msg attrs
