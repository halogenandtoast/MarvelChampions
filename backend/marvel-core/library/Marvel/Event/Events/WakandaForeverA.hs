module Marvel.Event.Events.WakandaForeverA
  ( wakandaForeverA
  , WakandaForeverA(..)
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Types
import Marvel.Event.Cards qualified as Cards
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Trait

wakandaForeverA :: EventCard WakandaForeverA
wakandaForeverA =
  event (WakandaForeverA . (`With` Meta mempty)) Cards.wakandaForeverA

newtype Meta = Meta { remaining :: HashSet UpgradeId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WakandaForeverA = WakandaForeverA (EventAttrs `With` Meta)
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor WakandaForeverA where
  getModifiersFor _ (UpgradeTarget uid) (WakandaForeverA (_ `With` meta))
    | HashSet.singleton uid == remaining meta = pure [LastSpecial]
  getModifiersFor _ _ _ = pure []

instance RunMessage WakandaForeverA where
  runMessage msg e@(WakandaForeverA (attrs `With` meta)) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ _ -> do
        upgradeIds <- select (UpgradeWithTrait BlackPanther)
        chooseOneAtATime
          identityId
          [ TargetLabel
              (UpgradeTarget upgradeId)
              [RunAbility (UpgradeTarget upgradeId) 1]
          | upgradeId <- HashSet.toList upgradeIds
          ]
        pure $ WakandaForeverA (attrs `With` Meta upgradeIds)
      _ -> WakandaForeverA . (`With` meta) <$> runMessage msg attrs
    RanAbility (UpgradeTarget upgradeId) 1 _
      | upgradeId `member` remaining meta -> pure $ WakandaForeverA
        (attrs `With` Meta (HashSet.delete upgradeId $ remaining meta))
    _ -> WakandaForeverA . (`With` meta) <$> runMessage msg attrs
