module Marvel.Treachery.Treacheries.TheVulturesPlans (
  theVulturesPlans,
  TheVulturesPlans (..),
) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

theVulturesPlans :: TreacheryCard TheVulturesPlans
theVulturesPlans =
  treachery (TheVulturesPlans . (`With` mempty)) Cards.theVulturesPlans

newtype Meta = Meta {unMeta :: HashSet Resource}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq, Semigroup, Monoid)

newtype TheVulturesPlans = TheVulturesPlans (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery TheVulturesPlans where
  toTreacheryAttrs (TheVulturesPlans (attrs `With` _)) = attrs

instance RunMessage TheVulturesPlans where
  runMessage msg t@(TheVulturesPlans (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        players <- getPlayers
        for_ players \identityId -> discardFor identityId attrs RandomFromHand 1 1
        pure t
      _ -> TheVulturesPlans . (`With` meta) <$> runMessage msg attrs
    WithDiscarded target _ cards | isTarget attrs target -> do
      let
        resources =
          HashSet.fromList (concatMap (printedResources . getCardDef) cards)
            `HashSet.difference` unMeta meta
      placeThreatOnEach attrs (fromIntegral $ HashSet.size resources) EncounterMainScheme
      pure $ TheVulturesPlans (attrs `With` Meta (unMeta meta <> resources))
    _ -> TheVulturesPlans . (`With` meta) <$> runMessage msg attrs
