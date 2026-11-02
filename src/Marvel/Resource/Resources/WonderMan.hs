module Marvel.Resource.Resources.WonderMan where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Card
import Marvel.Game.Source
import Marvel.Damage
import Marvel.Payment
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Resource.Types

-- The optional interrupt pays a real damage cost. ActiveCost tracks actual damage
-- through prevention and replacement windows, so prevented damage buys no icons.
energySiphonResources :: ReverseQueue m => IdentityId -> ActiveCostId -> PlayerCard -> m ()
energySiphonResources you acid card = do
  hero <- identityMatches HeroIdentity you
  if not hero then spendCardWithResources acid card [Energy] [Energy] else do
    remaining <- getCharacterRemainingHitPoints $ IdentityCharacter you
    amounts <- filterM (\n -> canAffordCost you (toSource card) $ DamageCost n) [1 .. min 3 remaining]
    chooseOne you $
      [labeled "$label.noResponse" $ spendCardWithResources acid card [Energy] [Energy]]
      <> [ labeled (countLabel "energySiphonDamage" n) $
            do
              paid (PendingEnergySiphonPayment card n 0)
              damageIdentity you card (toDamage n FromAbility)
              cardSpecific (ActiveCostRef acid) "wmFinishEnergySiphon" (you, card, n)
         | n <- amounts
         ]
