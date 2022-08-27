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

newtype SonicBoom = SonicBoom (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage SonicBoom where
  runMessage msg t@(SonicBoom attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        resources <- getAvailableResourcesFor Nothing
        allies <- selectList (AllyControlledBy $ IdentityWithId identityId)
        let
          goPay [] bs = ([], bs)
          goPay as [] = (as, [])
          goPay (a' : as) bs = case (a' `elem` bs, Wild `elem` bs) of
            (True, _) -> goPay as (L.delete a' bs)
            (_, True) -> goPay as (L.delete Wild bs)
            (False, False) -> (a' : as, bs)
          (uncovered, _) = goPay [Energy, Mental, Physical] resources
        chooseOrRunOne
          identityId
          (Label
              "Exhaust each character you control"
              [ Run
                $ IdentityMessage identityId ExhaustedIdentity
                : map (\a -> AllyMessage a ExhaustedAlly) allies
              ]
          : [ Label
                "Spend {energy}{mental}{physical} resources"
                [ Run
                    [ SetActiveCost $ ActiveCost
                        identityId
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
      IdentityMessage identityId (IdentityDamaged _ _) -> do
        push $ IdentityMessage identityId IdentityStunned
        pure t
      _ -> SonicBoom <$> runMessage msg attrs
    _ -> SonicBoom <$> runMessage msg attrs
