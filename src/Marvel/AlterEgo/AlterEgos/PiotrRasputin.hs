module Marvel.AlterEgo.AlterEgos.PiotrRasputin where

import Marvel.Prelude
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
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Upgrade.Cards qualified as Upgrades

piotrRasputin :: AlterEgoCard PiotrRasputin
piotrRasputin = alterEgo PiotrRasputin Cards.piotrRasputin (HP $ Static 14) (HandSize 6) (Rec 4) [Obligations.homesick]

newtype PiotrRasputin = PiotrRasputin (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PiotrRasputin where
  getAbilities (PiotrRasputin a) =
    [ ability a 1 Setup IsSelf NoCost (runAbility a 1)
    , limitedWindowAbility a 2 (W.IdentityChangedToForm W.After $ IdentityWithId $ alterEgoIdentityId a) Response (IsSelf <> ExtendedCardExists (InDiscardOf (IdentityWithId $ alterEgoIdentityId a) $ BasicCardMatches colossusCard)) NoCost (runAbility a 2)
    ]

instance RunMessage PiotrRasputin where
  runMessage msg e@(PiotrRasputin attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardIs Upgrades.organicSteel) SearchDrawOne ShuffleBackIn
      pure e
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ colossusCard)
      unless (null cards) $ chooseOne you [targeting c (shuffleIntoDeck you [c]) | c <- cards]
      pure e
    _ -> PiotrRasputin <$> runMessage msg attrs

colossusCard :: CardMatcher
colossusCard = CardMatchesAny $ map CardIs
  [ Allies.shadowcatKittyPryde, Supports.piotrsStudio, Upgrades.ironWill, Upgrades.titaniumMuscles
  , Upgrades.organicSteel, Events.madeOfRage, Events.steelFist, Events.bulletproofProtector, Events.armorUp
  ]
