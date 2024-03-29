module Marvel.Event.Events.WakandaForeverC (
  wakandaForeverC,
  WakandaForeverC (..),
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

wakandaForeverC :: EventCard WakandaForeverC
wakandaForeverC =
  event (WakandaForeverC . (`With` Meta mempty)) Cards.wakandaForeverC

newtype Meta = Meta {remaining :: HashSet UpgradeId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WakandaForeverC = WakandaForeverC (Attrs Event `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsEvent WakandaForeverC where
  toEventAttrs (WakandaForeverC (attrs `With` _)) = attrs

instance HasModifiersFor WakandaForeverC where
  getModifiersFor _ (UpgradeRef uid) (WakandaForeverC (_ `With` meta))
    | HashSet.singleton uid == remaining meta = pure [LastSpecial]
  getModifiersFor _ _ _ = pure []

instance RunMessage WakandaForeverC where
  runMessage msg (WakandaForeverC (attrs `With` meta)) = case msg of
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
        pure $ WakandaForeverC (attrs `With` Meta upgradeIds)
      _ -> WakandaForeverC . (`With` meta) <$> runMessage msg attrs
    RanAbility _ (UpgradeRef upgradeId) 1 _ _
      | upgradeId `member` remaining meta ->
          pure $
            WakandaForeverC
              (attrs `With` Meta (HashSet.delete upgradeId $ remaining meta))
    _ -> WakandaForeverC . (`With` meta) <$> runMessage msg attrs
