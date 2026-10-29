module Marvel.Event.Events.SneakAttack
  (sneakAttack, SneakAttack (..), sneakAttackEffect, SneakAttackEffect (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref

sneakAttack :: EventCard SneakAttack
sneakAttack = event SneakAttack Cards.sneakAttack

newtype SneakAttack = SneakAttack (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

sneakCandidates :: (Query m ExtendedCardMatcher) => Attrs Event -> m [PlayerCard]
sneakCandidates attrs = do
  cards <- selectList $ InHandOf (IdentityWithId $ eventController attrs) $
    BasicCardMatches (CardWithType AllyType) <> CanEnterPlay
  filterM (\card -> do
    gained <- getGainedTraits card
    anyM (\trait -> selectAny $ IdentityWithId (eventController attrs) <> IdentityWithTrait trait)
      (toList $ cdTraits (getCardDef card) <> gained)) cards

instance IsEvent SneakAttack where
  eventTargets (SneakAttack attrs) = Just . map toTarget <$> sneakCandidates attrs

instance RunMessage SneakAttack where
  runMessage msg e@(SneakAttack attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      -- The ally was chosen as a cost. Do not choose another or re-check a trait
      -- that was legal when the cost was paid if the identity has since changed form.
      cards <- selectList $ InHandOf (IdentityWithId you) $ BasicCardMatches (CardWithType AllyType) <> CanEnterPlay
      for_ [card | Just (CardIdRef cid) <- [paymentTarget payment], card <- cards, pcCardId card == cid] \card -> do
        let aid = AllyId $ unCardId $ pcCardId card
        createEffect Cards.sneakAttack aid (AllyEntity $ AllyWithId aid)
        putIntoPlay you card
      pure e
    _ -> SneakAttack <$> runMessage msg attrs

-- One independent delayed effect per play, even if the same event is played twice.
newtype SneakAttackEffect = SneakAttackEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

sneakAttackEffect :: CardEffect SneakAttackEffect
sneakAttackEffect = effect SneakAttackEffect Cards.sneakAttack

instance RunMessage SneakAttackEffect where
  runMessage msg e@(SneakAttackEffect attrs) = runQueueT $ case msg of
    EndPhase _ -> do
      for_ tracked \aid -> do
        alive <- getsGame (\g -> isJust $ cardInPlay g $ AllyRef aid)
        when alive $ removeFromPlay aid
        -- A character treated as a minion did not leave play (RR Ally/Minion).
        converted <- getsGame (\g -> isJust $ cardInPlay g $ MinionRef $ MinionId $ unAllyId aid)
        when converted $ removeFromPlay (MinionId $ unAllyId aid)
      disableEffect (effectId attrs)
      pure e
    RemoveFromPlay target | follows target -> e <$ disableEffect (effectId attrs)
    RemoveFromGame target | follows target -> e <$ disableEffect (effectId attrs)
    ReturnToHand target | follows target -> e <$ disableEffect (effectId attrs)
    _ -> SneakAttackEffect <$> runMessage msg attrs
   where
    tracked = case effectSource attrs of AllyRef aid -> Just aid; _ -> Nothing
    follows target = any (\aid -> case target of
      AllyRef a -> a == aid
      MinionRef m -> unMinionId m == unAllyId aid
      CardIdRef c -> unCardId c == unAllyId aid
      CharacterRef (AllyCharacter a) -> a == aid
      EnemyRef (EnemyMinionId m) -> unMinionId m == unAllyId aid
      _ -> False) tracked
