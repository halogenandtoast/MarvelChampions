module Marvel.Event.Events.WakandaForeverD
  ( wakandaForeverD
  , WakandaForeverD(..)
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

wakandaForeverD :: EventCard WakandaForeverD
wakandaForeverD =
  event (WakandaForeverD . (`With` Meta mempty)) Cards.wakandaForeverD

newtype Meta = Meta { remaining :: HashSet UpgradeId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WakandaForeverD = WakandaForeverD (EventAttrs `With` Meta)
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor WakandaForeverD where
  getModifiersFor _ (UpgradeTarget uid) (WakandaForeverD (_ `With` meta))
    | HashSet.singleton uid == remaining meta = pure [LastSpecial]
  getModifiersFor _ _ _ = pure []

instance RunMessage WakandaForeverD where
  runMessage msg e@(WakandaForeverD (attrs `With` meta)) = case msg of
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
        pure $ WakandaForeverD (attrs `With` Meta upgradeIds)
      _ -> WakandaForeverD . (`With` meta) <$> runMessage msg attrs
    RanAbility (UpgradeTarget upgradeId) 1 _
      | upgradeId `member` remaining meta -> pure $ WakandaForeverD
        (attrs `With` Meta (HashSet.delete upgradeId $ remaining meta))
    _ -> WakandaForeverD . (`With` meta) <$> runMessage msg attrs
