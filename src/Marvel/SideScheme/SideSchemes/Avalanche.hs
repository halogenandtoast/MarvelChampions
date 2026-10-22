module Marvel.SideScheme.SideSchemes.Avalanche (
  avalanche,
  Avalanche (..),
) where

import Marvel.Prelude

import Marvel.Cost
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

avalanche :: SideSchemeCard Avalanche
avalanche = sideScheme Avalanche Cards.avalanche (PerPlayer 2)

newtype Avalanche = Avalanche (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: each player spends an [energy] resource or takes 2 indirect damage (3
-- with 5 or more delay counters on the main scheme).
instance RunMessage Avalanche where
  runMessage msg s@(Avalanche attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      delay <- getYourMainSchemeCounters
      let n = if delay >= 5 then 3 else 2
          cost = MultiResourceCost [Just Energy]
      players <- getPlayers
      for_ players \you -> do
        canPay <- canAffordCost you (toSource attrs) cost
        chooseOrRunOne you $
          labeled "$label.takeIndirectDamage" (dealIndirectDamage you attrs n)
            : [labeled "$label.spendAnEnergyResource" (payForTreachery you cost) | canPay]
      pure s
    _ -> Avalanche <$> runMessage msg attrs
