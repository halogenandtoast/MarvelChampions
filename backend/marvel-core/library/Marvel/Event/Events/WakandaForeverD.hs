module Marvel.Event.Events.WakandaForeverD (
  wakandaForeverD,
  WakandaForeverD (..),
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
import Marvel.Ref
import Marvel.Trait

wakandaForeverD :: EventCard WakandaForeverD
wakandaForeverD =
  event (WakandaForeverD . (`With` Meta mempty)) Cards.wakandaForeverD

newtype Meta = Meta {remaining :: HashSet UpgradeId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WakandaForeverD = WakandaForeverD (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent WakandaForeverD where
  toEventAttrs (WakandaForeverD (attrs `With` _)) = attrs

instance HasModifiersFor WakandaForeverD where
  getModifiersFor _ (UpgradeRef uid) (WakandaForeverD (_ `With` meta))
    | HashSet.singleton uid == remaining meta = pure [LastSpecial]
  getModifiersFor _ _ _ = pure []

instance RunMessage WakandaForeverD where
  runMessage msg (WakandaForeverD (attrs `With` meta)) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        upgradeIds <- select (UpgradeWithTrait BlackPanther)
        chooseOneAtATime
          identityId
          [ TargetLabel
            (toRef upgradeId)
            [RunAbility (toRef upgradeId) 1]
          | upgradeId <- HashSet.toList upgradeIds
          ]
        pure $ WakandaForeverD (attrs `With` Meta upgradeIds)
      _ -> WakandaForeverD . (`With` meta) <$> runMessage msg attrs
    RanAbility (UpgradeRef upgradeId) 1 _ _
      | upgradeId `member` remaining meta ->
          pure $
            WakandaForeverD
              (attrs `With` Meta (HashSet.delete upgradeId $ remaining meta))
    _ -> WakandaForeverD . (`With` meta) <$> runMessage msg attrs
