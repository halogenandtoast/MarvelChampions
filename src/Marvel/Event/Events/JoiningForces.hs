module Marvel.Event.Events.JoiningForces where

import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Queue (HasQueue)
import Marvel.Projection (Projection)
import Marvel.Identity.Types (PlayerIdentity)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W
import Marvel.Identity (uniqueMatch)
import Marvel.Name (Name (..))
joiningForces :: EventCard JoiningForces
joiningForces = event JoiningForces Cards.joiningForces

newtype JoiningForces = JoiningForces (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

alliesInHand :: (HasGame m, MonadThrow m, HasQueue m, Projection m PlayerIdentity) => Trait.Trait -> m [PlayerCard]
alliesInHand trait = do
  cards <- selectList (InHandOf AnyIdentity $ BasicCardMatches $ CardWithType AllyType <> CardWithTrait trait)
  inPlay <- getUniqueTitlesInPlay
  let allowed card =
        let def = getCardDef card
        in not (cdUnique def) || not (any (uniqueMatch (title $ cdName def, maybeToList $ subtitle $ cdName def)) inPlay)
  pure (filter allowed cards)

instance IsEvent JoiningForces where
  eventTargets _ = do
    avengers <- alliesInHand Trait.Avenger
    guardians <- alliesInHand Trait.Guardian
    pure $ if null avengers && null guardians then Just [] else Nothing

instance RunMessage JoiningForces where
  runMessage msg e@(JoiningForces attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      allies <- alliesInHand Trait.Avenger
      unless (null allies) $ chooseOne you [targeting c (putIntoPlay (fromMaybe you $ pcOwner c) c) | c <- allies]
      cardSpecific attrs "putGuardian" you
      pure e
    CardSpecific (isTarget attrs -> True) "putGuardian" (fromJSON -> Success you) -> do
      allies <- alliesInHand Trait.Guardian
      unless (null allies) $ chooseOne you [targeting c (putIntoPlay (fromMaybe you $ pcOwner c) c) | c <- allies]
      pure e
    _ -> JoiningForces <$> runMessage msg attrs
