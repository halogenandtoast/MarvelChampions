module Marvel.Treachery.Treacheries.TheVulturesPlans
  ( theVulturesPlans
  , TheVulturesPlans(..)
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

theVulturesPlans :: TreacheryCard TheVulturesPlans
theVulturesPlans =
  treachery (TheVulturesPlans . (`With` mempty)) Cards.theVulturesPlans

newtype Meta = Meta { unMeta :: HashSet Resource }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq, Semigroup, Monoid)

newtype TheVulturesPlans = TheVulturesPlans (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance IsTreachery TheVulturesPlans where
  toTreacheryAttrs (TheVulturesPlans (attrs `With` _)) = attrs

instance RunMessage TheVulturesPlans where
  runMessage msg t@(TheVulturesPlans (attrs `With` meta)) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        players <- getPlayers
        pushAll $ map
          (\identityId -> IdentityMessage identityId
            $ DiscardFor (toTarget attrs) RandomFromHand 1 1
          )
          players
        pure t
      _ -> TheVulturesPlans . (`With` meta) <$> runMessage msg attrs
    WithDiscarded target _ cards | isTarget attrs target -> do
      let
        resources =
          HashSet.fromList (concatMap (printedResources . getCardDef) cards)
            `HashSet.difference` unMeta meta
      identityId <- selectJust You
      msgs <- choiceMessages
        identityId
        (PlaceThreat
          (toSource attrs)
          (fromIntegral $ HashSet.size resources)
          MainScheme
        )
      pushAll msgs
      pure $ TheVulturesPlans (attrs `With` Meta (unMeta meta <> resources))
    _ -> TheVulturesPlans . (`With` meta) <$> runMessage msg attrs
