module Marvel.Event.Events.MagicBarrier where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Card
import Marvel.Damage
import Marvel.Deck
import Marvel.Identity.Types
import Marvel.Projection
import Marvel.Resource.Types
import Marvel.Window qualified as W

magicBarrier :: EventCard MagicBarrier
magicBarrier = event MagicBarrier Cards.magicBarrier

newtype MagicBarrier = MagicBarrier (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MagicBarrier where
  runMessage msg e@(MagicBarrier attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just window)) | eid == eventId attrs -> do
      let attackers = [enemy | W.EnemyAttack enemy _ <- [window]]
            <> [enemy | W.EnemyAttackAlly enemy _ _ <- [window]]
      for_ attackers \enemy -> do
        preventEnemyAttackDamage enemy 3
        deck <- projectMap PlayerIdentityDeck unDeck you
        when (maybe False (any (`elem` [Energy, Wild]) . printedResources . getCardDef) (headMay deck)) $
          dealDamage enemy attrs (toDamage 3 FromAbility)
      pure e
    _ -> MagicBarrier <$> runMessage msg attrs
