module Marvel.Support.Supports.SoulWorld (
  soulWorld,
  SoulWorld (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Window qualified as W

soulWorld :: SupportCard SoulWorld
soulWorld = support SoulWorld Cards.soulWorld

newtype SoulWorld = SoulWorld (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: After your deck runs out of cards (it is reset, R19), place 1 soul counter
-- here. Alter-Ego Action: Exhaust Soul World and remove 1 soul counter from it → heal
-- all damage from your identity. Its soul counters are its uses.
instance HasAbilities SoulWorld where
  getAbilities a@(SoulWorld attrs) =
    [ limitedWindowAbility a 1 (W.PlayerDeckResets W.After (IdentityWithId $ supportController attrs)) Response OwnsThis NoCost (runAbility a 1)
    , ability a 2 AlterEgoAction (OwnsThis <> SelfMatches IdentityWithAnyDamage) (ExhaustCost <> UseCost) (runAbility a 2)
    ]

instance RunMessage SoulWorld where
  runMessage msg s@(SoulWorld attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> pure $ SoulWorld $ attrs & usesL +~ 1
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      damage <- projectMap PlayerIdentityDamage id you
      heal you damage
      pure s
    _ -> SoulWorld <$> runMessage msg attrs
