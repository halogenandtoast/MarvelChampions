module Marvel.Support.Supports.DeadpoolCorpsShip where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Aspect
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity (uniqueMatch)
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers ()
import Marvel.Name
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

deadpoolCorpsShip :: SupportCard DeadpoolCorpsShip
deadpoolCorpsShip = support DeadpoolCorpsShip Cards.deadpoolCorpsShip

newtype DeadpoolCorpsShip = DeadpoolCorpsShip (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DeadpoolCorpsShip where
  getAbilities a =
    [ability a 1 Action (OwnsThis <> PoolAllyInHandCanEnterPlay)
      (ExhaustCost <> DealEncounterCardCost) (runAbility a 1)]

instance RunMessage DeadpoolCorpsShip where
  runMessage msg e@(DeadpoolCorpsShip attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand you
      inPlay <- getUniqueTitlesInPlay
      let clashes card = cdUnique (getCardDef card)
            && any (uniqueMatch (title $ cdName $ getCardDef card, maybe [] pure (subtitle $ cdName $ getCardDef card))) inPlay
          allies = filter (\card -> cardMatch (CardWithType AllyType <> CardWithAspect Pool) card && not (clashes card)) hand
      chooseOne you [targeting card (putIntoPlay you card) | card <- allies]
      pure e
    _ -> DeadpoolCorpsShip <$> runMessage msg attrs
