-- | Tower Defense's minion (The Mad Titan's Shadow).
module Marvel.Minion.Minions.BlackOrderBesieger (
  blackOrderBesieger,
  BlackOrderBesieger (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Damage
import Marvel.Environment.Environments.AvengersTower (damageAvengersTower)
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

blackOrderBesieger :: MinionCard BlackOrderBesieger
blackOrderBesieger = minion BlackOrderBesieger Cards.blackOrderBesieger (Sch 1) (Atk 2) (HP 3)

newtype BlackOrderBesieger = BlackOrderBesieger (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: after Black Order Besieger engages you, choose to either deal 1
-- damage to Avengers Tower or deal 2 damage to your identity. Put into play engaged with
-- you, it has engaged you (RR Engage).
instance HasAbilities BlackOrderBesieger where
  getAbilities (BlackOrderBesieger a) =
    [windowAbility a 1 (EngagesMinion After AnyIdentity (MinionWithId $ minionId a)) ForcedResponse NoCost $ runAbility a 1]

instance RunMessage BlackOrderBesieger where
  runMessage msg e@(BlackOrderBesieger attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      chooseOne you
        [ labeled (countLabel "damageAvengersTower" 1) (damageAvengersTower 1)
        , labeled (countLabel "takeDamage" 2) (damageIdentity you attrs (toDamage 2 FromAbility))
        ]
      pure e
    _ -> BlackOrderBesieger <$> runMessage msg attrs
