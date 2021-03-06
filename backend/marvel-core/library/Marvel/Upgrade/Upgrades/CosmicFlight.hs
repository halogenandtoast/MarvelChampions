module Marvel.Upgrade.Upgrades.CosmicFlight (
  cosmicFlight,
  CosmicFlight (..),
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
import Marvel.Upgrade.Attrs
import qualified Marvel.Upgrade.Cards as Cards
import Marvel.Window

cosmicFlight :: UpgradeCard CosmicFlight
cosmicFlight = upgrade CosmicFlight Cards.cosmicFlight

newtype CosmicFlight = CosmicFlight UpgradeAttrs
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

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
  [ IdentityMessage
    ident
    ( IdentityDamaged
        source
        (dmg {damageAmount = max 0 $ damageAmount dmg - 3})
    )
  | damageAmount dmg > 3
  ]
decreaseDamage _ = error "Invalid message"

instance RunMessage CosmicFlight where
  runMessage msg a = case msg of
    RanAbility (isTarget a -> True) 1 [IdentityTakeDamage ident _] -> do
      replaceMatchingMessage decreaseDamage $ \case
        IdentityMessage identityId' (IdentityDamaged _ _) ->
          identityId' == ident
        _ -> False
      pure a
    _ -> CosmicFlight <$> runMessage msg (toAttrs a)
