module Marvel.Treachery.Treacheries.SonicBoom (
  sonicBoom,
  SonicBoom (..),
) where

import Marvel.Prelude

import Data.List qualified as L
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

sonicBoom :: TreacheryCard SonicBoom
sonicBoom = treachery SonicBoom Cards.sonicBoom

newtype SonicBoom = SonicBoom (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SonicBoom where
  runMessage msg t@(SonicBoom attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        resources <- getAvailableResourcesFor identityId Nothing
        allies <- selectList (AllyControlledBy $ IdentityWithId identityId)
        let
          goPay [] bs = ([], bs)
          goPay as [] = (as, [])
          goPay (a' : as) bs = case (a' `elem` bs, Wild `elem` bs) of
            (True, _) -> goPay as (L.delete a' bs)
            (_, True) -> goPay as (L.delete Wild bs)
            (False, False) -> (a' : as, bs)
          (uncovered, _) = goPay [Energy, Mental, Physical] resources
        chooseOrRunOne identityId $
          labeled "$label.exhaustEachCharacterYouControl" (exhaust identityId >> for_ allies exhaust)
            : [ labeled "$label.spendEnergyMentalPhysicalResources" $
                  payForTreachery identityId (MultiResourceCost [Just Energy, Just Mental, Just Physical])
              | null uncovered
              ]
        pure t
      _ -> SonicBoom <$> runMessage msg attrs
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> pure t
      IdentityMessage identityId (IdentityDamaged _ _) -> do
        exhaust identityId
        pure t
      _ -> SonicBoom <$> runMessage msg attrs
    _ -> SonicBoom <$> runMessage msg attrs
