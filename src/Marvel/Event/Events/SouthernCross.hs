module Marvel.Event.Events.SouthernCross where

import Marvel.Prelude
import Marvel.Id
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Damage
import Marvel.Keyword
import Marvel.Trait qualified as Trait

southernCross :: EventCard SouthernCross
southernCross = event SouthernCross Cards.southernCross

newtype SouthernCross = SouthernCross (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SouthernCross where
  runMessage msg e@(SouthernCross attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      aerial <- member Trait.Aerial <$> getCharacterTraits (IdentityCharacter you)
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        dealDamage enemy attrs (toDamage (if aerial then 8 else 6) $ FromPlayerAttack you)
        cardSpecific attrs "southernCrossBonuses" (you, enemy)
        | enemy <- enemies]
      pure e
    CardSpecific (isTarget attrs -> True) "southernCrossBonuses" (fromJSON -> Success (you, enemy :: EnemyId)) -> do
      printed <- getsGame (fmap (toList . cdKeywords) . (`abilityCardDef` IdentityRef you))
      modifiers <- getModifiers (IdentityRef you)
      let keywords = fromMaybe [] printed <> [keyword | KeywordModifier keyword <- modifiers]
      when (any (\case Retaliate n -> n > 0; _ -> False) keywords) $ stun enemy attrs
      when (Stalwart `elem` keywords) $ drawCards you 1
      pure e
    _ -> SouthernCross <$> runMessage msg attrs
