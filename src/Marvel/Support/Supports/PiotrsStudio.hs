module Marvel.Support.Supports.PiotrsStudio where

import Marvel.Prelude
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Event.Cards qualified as Events
import Marvel.Support.Cards qualified as Supports
import Marvel.Ally.Cards qualified as Allies
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

piotrsStudio :: SupportCard PiotrsStudio
piotrsStudio = support PiotrsStudio Cards.piotrsStudio

newtype PiotrsStudio = PiotrsStudio (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PiotrsStudio where
  getAbilities (PiotrsStudio a) =
    [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage PiotrsStudio where
  runMessage msg e@(PiotrsStudio attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ discardFromDeckUntil you (colossusCard) attrs
    WithDiscardedMatch (isTarget attrs -> True) _ card -> do
      for_ (onlyPlayerCards [card]) (addToHand $ supportController attrs)
      pure e
    _ -> PiotrsStudio <$> runMessage msg attrs

colossusCard :: CardMatcher
colossusCard = CardMatchesAny $ map CardIs
  [ Allies.shadowcatKittyPryde, Supports.piotrsStudio, Upgrades.ironWill, Upgrades.titaniumMuscles
  , Upgrades.organicSteel, Events.madeOfRage, Events.steelFist, Events.bulletproofProtector, Events.armorUp
  ]
