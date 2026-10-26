module Marvel.Event.Events.WaspSting (
  waspSting,
  WaspSting (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait (Trait (Giant))

waspSting :: EventCard WaspSting
waspSting = event WaspSting Cards.waspSting

newtype WaspSting = WaspSting (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Two Hero Actions, one per hero form (the def's criteria: Giant or Tiny): in Giant
-- form a total of 4 damage divided among enemies, each attacked; in Tiny form 5 damage
-- to an enemy.
instance RunMessage WaspSting where
  runMessage msg e@(WaspSting attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      giant <- identityMatches (IdentityWithTrait Giant) you
      if giant
        then dealDividedDamage you attrs (toDamage 4 (FromPlayerAttack you)) AttackableEnemy
        else chooseDamage you attrs (toDamage 5 (FromPlayerAttack you)) AttackableEnemy
      pure e
    _ -> WaspSting <$> runMessage msg attrs
