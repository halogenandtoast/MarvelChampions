module Marvel.Event.Events.MutantProtectors where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

mutantProtectors :: EventCard MutantProtectors
mutantProtectors = event MutantProtectors Cards.mutantProtectors

newtype MutantProtectors = MutantProtectors (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MutantProtectors where
  runMessage msg e@(MutantProtectors attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.EnemyAttack enemy _))) | eid == eventId attrs -> do
      cards <- selectList (InHandOf (IdentityWithId you) $ BasicCardMatches (CardWithType AllyType <> CardWithTrait Trait.XMen))
      chooseOne you [targeting card (putIntoPlay you card >> cardSpecific attrs "defend" (you, enemy, pcCardId card)) | card <- cards]
      pure e
    CardSpecific (isTarget attrs -> True) "defend" (fromJSON -> Success (you :: IdentityId, enemy :: EnemyId, cid :: CardId)) -> do
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      let found = [aid | aid <- allies, aid == AllyId (unCardId cid)]
      for_ (headMay found) \aid -> do
        exhaust aid
        enemyDefendedBy enemy (AllyCharacter aid)
      pure e
    _ -> MutantProtectors <$> runMessage msg attrs
