module Marvel.Treachery.Treacheries.SonicBoom
  ( sonicBoom
  , SonicBoom(..)
  ) where

import Marvel.Prelude

import Data.List qualified as L
import Marvel.ActiveCost
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Message
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

sonicBoom :: TreacheryCard SonicBoom
sonicBoom = treachery SonicBoom Cards.sonicBoom

newtype SonicBoom = SonicBoom TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage SonicBoom where
  runMessage msg t@(SonicBoom attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery ident -> do
          resources <- getAvailableResourcesFor Nothing
          allies <- selectList (AllyControlledBy $ IdentityWithId ident)
          let
            goPay [] bs = ([], bs)
            goPay as [] = (as, [])
            goPay (a' : as) bs = case (a' `elem` bs, Wild `elem` bs) of
              (True, _) -> goPay as (L.delete a' bs)
              (_, True) -> goPay as (L.delete Wild bs)
              (False, False) -> (a' : as, bs)
            (uncovered, _) = goPay [Energy, Mental, Physical] resources
          chooseOrRunOne
            ident
            (Label
                "Exhaust each character you control"
                [ Run
                  $ IdentityMessage ident ExhaustedIdentity
                  : map (\a -> AllyMessage a ExhaustedAlly) allies
                ]
            : [ Label
                  "Spend {energy}{mental}{physical} resources"
                  [ Run
                      [ SetActiveCost $ ActiveCost
                          ident
                          ForTreachery
                          (MultiResourceCost
                            [Just Energy, Just Mental, Just Physical]
                          )
                          NoPayment
                          Nothing
                          mempty
                      ]
                  ]
              | null uncovered
              ]
            )
          pure t
        _ -> SonicBoom <$> runMessage msg attrs
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> pure t
      IdentityMessage ident (IdentityDamaged _ _) -> do
        push $ IdentityMessage ident IdentityStunned
        pure t
      _ -> SonicBoom <$> runMessage msg attrs
    _ -> SonicBoom <$> runMessage msg attrs
