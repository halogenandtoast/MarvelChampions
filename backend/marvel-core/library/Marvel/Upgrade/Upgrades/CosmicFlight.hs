module Marvel.Upgrade.Upgrades.CosmicFlight
  ( cosmicFlight
  , CosmicFlight(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

cosmicFlight :: UpgradeCard CosmicFlight
cosmicFlight = upgrade CosmicFlight Cards.cosmicFlight

newtype CosmicFlight = CosmicFlight (Attrs Upgrade)
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasModifiersFor CosmicFlight where
  getModifiersFor _ (IdentityTarget ident) (CosmicFlight attrs)
    | ident == upgradeController attrs = pure [TraitModifier Aerial]
  getModifiersFor _ _ _ = pure []

instance HasAbilities CosmicFlight where
  getAbilities a =
    [ limitedWindowAbility
          a
          1
          (WouldTakeDamage You FromAnyDamageSource AnyValue)
          Response
          OwnsThis
          NoCost
        $ runAbility a 1
    ]

decreaseDamage :: Message -> [Message]
decreaseDamage (IdentityMessage ident (IdentityDamaged source dmg)) =
  [ IdentityMessage ident $ IdentityDamaged
      source
      (dmg { damageAmount = max 0 $ damageAmount dmg - 3 })
  | damageAmount dmg > 3
  ]
decreaseDamage _ = error "Invalid message"

instance RunMessage CosmicFlight where
  runMessage msg a@(CosmicFlight attrs) = case msg of
    RanAbility (isTarget a -> True) 1 [IdentityTakeDamage ident _] _ -> do
      replaceMatchingMessage decreaseDamage $ \case
        IdentityMessage identityId' (IdentityDamaged _ _) ->
          identityId' == ident
        _ -> False
      pure a
    _ -> CosmicFlight <$> runMessage msg attrs
