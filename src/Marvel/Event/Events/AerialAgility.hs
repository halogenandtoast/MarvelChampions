module Marvel.Event.Events.AerialAgility where

import Marvel.Prelude
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Card.Side ()
import Marvel.Damage ()
import Marvel.Effect.Types
import Marvel.Keyword
import Marvel.Entity ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Ref
import Marvel.Window qualified as W

aerialAgility :: EventCard AerialAgility
aerialAgility = event AerialAgility Cards.aerialAgility

newtype AerialAgility = AerialAgility (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AerialAgility where
  runMessage msg e@(AerialAgility attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just window)) | eid == eventId attrs -> do
      let attacker = case window of
            W.EnemyAttack enemy _ -> Just enemy
            W.EnemyAttackAlly enemy _ _ -> Just enemy
            _ -> Nothing
      for_ attacker $ \enemy -> do
        angel <- identityMatches (IdentityWithTitle "Angel") you
        if angel
          then createEffect Cards.aerialAgility (enemyRef enemy) (EnemyEntity $ EnemyWithId enemy)
          else do
            becomeTough you
            createEffect Cards.aerialAgility (enemyRef enemy) (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> AerialAgility <$> runMessage msg attrs

newtype AerialAgilityEffect = AerialAgilityEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

aerialAgilityEffect :: CardEffect AerialAgilityEffect
aerialAgilityEffect = initialize <$> effect AerialAgilityEffect Cards.aerialAgility
 where
  initialize (AerialAgilityEffect attrs) = AerialAgilityEffect attrs
    {effectModifiers = case effectMatcher attrs of
      EnemyEntity _ -> [IgnoreBoostCards]
      IdentityEntity _ -> [KeywordModifier $ Retaliate 1]
      _ -> []}

instance RunMessage AerialAgilityEffect where
  runMessage msg e@(AerialAgilityEffect attrs) = runQueueT $ case msg of
    CheckWindows windows | any ended windows -> e <$ disableEffect (effectId attrs)
    _ -> AerialAgilityEffect <$> runMessage msg attrs
   where
    ended (W.Window W.After (W.EnemyAttack enemy _)) = toSource (enemyRef enemy) == effectSource attrs
    ended (W.Window W.After (W.EnemyAttackAlly enemy _ _)) = toSource (enemyRef enemy) == effectSource attrs
    ended _ = False
