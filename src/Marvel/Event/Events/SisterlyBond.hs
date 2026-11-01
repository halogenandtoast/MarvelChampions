module Marvel.Event.Events.SisterlyBond where

import Marvel.Prelude
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity.Types (Field (PlayerIdentitySide, PlayerIdentitySides), PlayerIdentitySide (..))
import Marvel.Projection
import Marvel.Effect.Types
import Marvel.Window qualified as W

sisterlyBond :: EventCard SisterlyBond
sisterlyBond = event SisterlyBond Cards.sisterlyBond

newtype SisterlyBond = SisterlyBond (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SisterlyBond where
  runMessage msg e@(SisterlyBond attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ _ window) | eid == eventId attrs -> do
      x23 <- selectList (IdentityWithTitle "X-23")
      for_ x23 $ \laura -> do
        side <- project PlayerIdentitySide laura
        sides <- project PlayerIdentitySides laura
        for_ [h | Just (HeroSide h) <- [lookup side sides]] $ \h -> do
          power <- case window of
            Just (W.AllyAttack aid enemy) -> do
              modifiers <- (<>) <$> getModifiers (AllyRef aid) <*> getModifiers (enemyRef enemy)
              pure $ Just (aid, AttackWithThwart `notElem` modifiers)
            Just (W.AllyThwart aid _) -> Just . (aid,) <$> allyThwartUsesAttack aid
            _ -> pure Nothing
          for_ power $ \(aid, usingAttack) -> do
            bonus <- if usingAttack then Hero.getModifiedAttack (toAttrs h) else Hero.getModifiedThwart (toAttrs h)
            createEffect Cards.sisterlyBond attrs (AllyEntity $ AllyWithId aid)
            cardSpecific attrs "sisterlyBondBonus" (usingAttack, bonus)
            addToAllyBasicPower aid bonus
      pure e
    _ -> SisterlyBond <$> runMessage msg attrs

newtype SisterlyBondEffect = SisterlyBondEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

sisterlyBondEffect :: CardEffect SisterlyBondEffect
sisterlyBondEffect = effect SisterlyBondEffect Cards.sisterlyBond

instance RunMessage SisterlyBondEffect where
  runMessage msg e@(SisterlyBondEffect attrs) = runQueueT $ case msg of
    AllyMessage aid (AllyAttackLands enemy _) | applies aid -> do
      alive <- (&&) <$> selectAny (AllyWithId aid) <*> selectAny (EnemyWithId enemy)
      unless alive $ disableEffect (effectId attrs)
      pure e
    IdentityMessage _ (AllyRemoved aid) | applies aid -> e <$ disableEffect (effectId attrs)
    CardSpecific source "sisterlyBondBonus" (fromJSON -> Success (usingAttack, bonus :: Natural))
      | refocus source == effectSource attrs -> pure $ SisterlyBondEffect $ attrs & modifiersL .~ [if usingAttack then AttackModifier bonus else ThwartModifier bonus]
    CheckWindows ws | any ends ws -> e <$ disableEffect (effectId attrs)
    _ -> SisterlyBondEffect <$> runMessage msg attrs
   where
    ends = \case
      W.Window W.After (W.AllyAttack aid _) -> applies aid
      W.Window W.After (W.AllyThwart aid _) -> applies aid
      W.Window W.After (W.AllyAttackCompletedAgainst aid _) -> applies aid
      _ -> False
    applies aid = effectMatcher attrs == AllyEntity (AllyWithId aid)
