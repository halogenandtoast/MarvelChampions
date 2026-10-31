module Marvel.Resource.Resources.MolecularAcceleration where

import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

-- | Counter generation a spending interrupt can provide. Affordability may inspect
-- this without executing the interrupt; the actual optional choice still happens
-- before the resource card contributes its payment.
spendingInterruptCounters :: HasGame m => IdentityId -> PlayerCard -> m [(IdentityId, Natural)]
spendingInterruptCounters you card
  | canonicalCardCode (getCardDef card) == "37010" = do
      hero <- identityMatches HeroIdentity you
      if hero then maybe [] (\who -> [(who, 1)]) <$> selectOne (HeroIdentity <> IdentityWithTitle "Gambit")
        else pure []
  | otherwise = pure []

molecularAccelerationSpent :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
molecularAccelerationSpent you card = do
  gains <- spendingInterruptCounters you card
  for_ gains $ \(who, n) -> chooseOne you
    [ labeled "$label.molecularAcceleration" (placeCounters who n)
    , labeled "$label.noResponse" (pure ())
    ]
