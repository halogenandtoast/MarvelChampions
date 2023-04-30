module Marvel.Event.Events.WakandaForeverA
  ( wakandaForeverA
  , WakandaForeverA(..)
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

wakandaForeverA :: EventCard WakandaForeverA
wakandaForeverA =
  event (WakandaForeverA . (`With` Meta mempty)) Cards.wakandaForeverA

newtype Meta = Meta { remaining :: HashSet UpgradeId }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WakandaForeverA = WakandaForeverA (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance IsEvent WakandaForeverA where
  toEventAttrs (WakandaForeverA (attrs `With` _)) = attrs

instance HasModifiersFor WakandaForeverA where
  getModifiersFor _ (UpgradeTarget uid) (WakandaForeverA (_ `With` meta))
    | HashSet.singleton uid == remaining meta = pure [LastSpecial]
  getModifiersFor _ _ _ = pure []

instance RunMessage WakandaForeverA where
  runMessage msg (WakandaForeverA (attrs `With` meta)) = case msg of
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
        pure $ WakandaForeverA (attrs `With` Meta upgradeIds)
      _ -> WakandaForeverA . (`With` meta) <$> runMessage msg attrs
    RanAbility (UpgradeTarget upgradeId) 1 _ _
      | upgradeId `member` remaining meta -> pure $ WakandaForeverA
        (attrs `With` Meta (HashSet.delete upgradeId $ remaining meta))
    _ -> WakandaForeverA . (`With` meta) <$> runMessage msg attrs
