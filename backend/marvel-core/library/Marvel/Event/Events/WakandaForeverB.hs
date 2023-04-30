module Marvel.Event.Events.WakandaForeverB
  ( wakandaForeverB
  , WakandaForeverB(..)
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Trait

wakandaForeverB :: EventCard WakandaForeverB
wakandaForeverB =
  event (WakandaForeverB . (`With` Meta mempty)) Cards.wakandaForeverB

newtype Meta = Meta { remaining :: HashSet UpgradeId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WakandaForeverB = WakandaForeverB (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance IsEvent WakandaForeverB where
  toEventAttrs (WakandaForeverB (attrs `With` _)) = attrs

instance HasModifiersFor WakandaForeverB where
  getModifiersFor _ (UpgradeTarget uid) (WakandaForeverB (_ `With` meta))
    | HashSet.singleton uid == remaining meta = pure [LastSpecial]
  getModifiersFor _ _ _ = pure []

instance RunMessage WakandaForeverB where
  runMessage msg (WakandaForeverB (attrs `With` meta)) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        upgradeIds <- select (UpgradeWithTrait BlackPanther)
        chooseOneAtATime
          identityId
          [ TargetLabel
              (UpgradeTarget upgradeId)
              [RunAbility (UpgradeTarget upgradeId) 1]
          | upgradeId <- HashSet.toList upgradeIds
          ]
        pure $ WakandaForeverB (attrs `With` Meta upgradeIds)
      _ -> WakandaForeverB . (`With` meta) <$> runMessage msg attrs
    RanAbility (UpgradeTarget upgradeId) 1 _ _
      | upgradeId `member` remaining meta -> pure $ WakandaForeverB
        (attrs `With` Meta (HashSet.delete upgradeId $ remaining meta))
    _ -> WakandaForeverB . (`With` meta) <$> runMessage msg attrs
