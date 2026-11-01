module Marvel.Event.Events.Cable where

import Marvel.Prelude
import Data.List (delete)
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Status ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Query
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Keyword qualified as Keyword ()
import Marvel.Effect.Types
import Marvel.Card.Side
import {-# SOURCE #-} Marvel.Game ()

import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Stats ()
import Marvel.GameValue ()
import Marvel.Hand ()

cable40002 :: EventCard Cable40002
cable40002 = event Cable40002 Cards.cable40002

newtype Cable40002 = Cable40002 (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40002 where
  runMessage msg a@(Cable40002 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      inHero <- identityMatches HeroIdentity you
      changeToForm you (if inHero then B else A)
      cardSpecific attrs "others" (you, if inHero then B else A)
      pure a
    CardSpecific (isTarget attrs -> True) "others" (fromJSON -> Success (you, form)) -> do
      players <- getPlayers
      for_ (filter (/= you) players) \other -> do
        same <- identityMatches (if form == A then HeroIdentity else AlterEgoIdentity) other
        unless same $ chooseOne other
          [ labeled "$label.changeForm" (changeToForm other form)
          , labeled "$label.done" (pure ())
          ]
      pure a
    _ -> Cable40002 <$> runMessage msg attrs

cable40003 :: EventCard Cable40003
cable40003 = event Cable40003 Cards.cable40003

newtype Cable40003 = Cable40003 (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40003 where
  runMessage msg a@(Cable40003 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      n <- sideSchemesInVictoryDisplay
      removeThreat you attrs (3 + n) ThwartableScheme
      pure a
    _ -> Cable40003 <$> runMessage msg attrs

cable40004 :: EventCard Cable40004
cable40004 = event Cable40004 Cards.cable40004

newtype Cable40004 = Cable40004 (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40004 where
  runMessage msg a@(Cable40004 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ _) | eid == eventId attrs -> do
      n <- sideSchemesInVictoryDisplay
      lookAtTopOfEncounterDeck n attrs
      pure a
    SearchFoundCards (isTarget attrs -> True) (onlyEncounterCards -> cards) -> do
      unless (null cards) $ lookingAt (eventController attrs) (EncounterCard <$> cards) $
        chooseOne (eventController attrs) $
          [targeting c $ discardCard c >> cardSpecific attrs "order" ([] :: [EncounterCard], delete c cards) | c <- cards]
          <> [labeled "$label.done" $ cardSpecific attrs "order" ([] :: [EncounterCard], cards)]
      pure a
    CardSpecific (isTarget attrs -> True) "order" (fromJSON -> Success (chosen, rest)) -> do
      if length rest <= 1
        then putOnTopOfEncounterDeck (chosen <> rest)
        else chooseOne (eventController attrs)
          [targeting c $ cardSpecific attrs "order" (chosen <> [c], delete c rest) | c <- rest]
      pure a
    _ -> Cable40004 <$> runMessage msg attrs

cable40005 :: EventCard Cable40005
cable40005 = event Cable40005 Cards.cable40005

newtype Cable40005 = Cable40005 (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40005 where
  runMessage msg a@(Cable40005 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      n <- sideSchemesInVictoryDisplay
      chooseDamage you attrs (toDamage (6 + n) $ FromPlayerAttack you) AttackableEnemy
      pure a
    _ -> Cable40005 <$> runMessage msg attrs

cable40017 :: EventCard Cable40017
cable40017 = event Cable40017 Cards.cable40017

newtype Cable40017 = Cable40017 (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40017 where
  runMessage msg a@(Cable40017 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      createEffect Cards.cable40017 attrs (AllyEntity $ AllyControlledBy $ IdentityWithId you)
      pure a
    _ -> Cable40017 <$> runMessage msg attrs

cable40026 :: EventCard Cable40026
cable40026 = event Cable40026 Cards.cable40026

newtype Cable40026 = Cable40026 (Attrs Event)
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40026 where
  runMessage msg a@(Cable40026 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      identities <- concatMapM (selectList . IdentityWithTitle) ["Cable", "Deadpool"]
      allies <- concatMapM (selectList . AllyWithTitle) ["Cable", "Deadpool"]
      for_ identities \p -> damageIdentity p attrs (toDamage 1 FromAbility)
      for_ allies \aid -> damageCharacter aid attrs (toDamage 1 FromAbility)
      schemes <- selectList SchemeYouCanThwart
      chooseOne you [targeting (schemeRef sid) $
        cardSpecific attrs "second" (you, sid, delete sid schemes) | sid <- schemes]
      pure a
    CardSpecific (isTarget attrs -> True) "second" (fromJSON -> Success (you, first :: SchemeId, otherSchemes :: [SchemeId])) -> do
      if null otherSchemes then thwartEachAs you attrs [(first, 3)] else
        chooseOne you [targeting (schemeRef second) $ thwartEachAs you attrs [(first, 3), (second, 3)] | second <- otherSchemes]
      pure a
    _ -> Cable40026 <$> runMessage msg attrs

newtype CableMissionPlanningEffect = CableMissionPlanningEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

cableMissionPlanningEffect :: CardEffect CableMissionPlanningEffect
cableMissionPlanningEffect = effectWith CableMissionPlanningEffect Cards.cable40017 (modifiersL .~ [NoConsequentialDamage])

instance RunMessage CableMissionPlanningEffect where
  runMessage msg a@(CableMissionPlanningEffect attrs) = runQueueT $ case msg of
    EndPhase _ -> a <$ disableEffect (effectId attrs)
    _ -> CableMissionPlanningEffect <$> runMessage msg attrs
