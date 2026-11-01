module Marvel.Environment.Environments.BadoonShip (
  badoonShip,
  BadoonShip (..),
  chargeUp,
) where

import Marvel.Prelude

import Marvel.Ability.Types
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref ()

-- | Put into play by Terrestrial Invasion's setup (Brotherhood of Badoon).
badoonShip :: EnvironmentCard BadoonShip
badoonShip = environment BadoonShip Cards.badoonShip

newtype BadoonShip = BadoonShip (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | Charge Up — Special: place 1 barrage counter here. Then, if there are 4 or more
-- barrage counters here, deal 2 indirect damage to each player and remove all barrage
-- counters from here.
instance RunMessage BadoonShip where
  runMessage msg e@(BadoonShip attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "chargeUp" _ -> do
      you <- getYou
      specialAbility you attrs do
        placeCounters attrs 1
        cardSpecific attrs "barrage" ()
      pure e
    CardSpecific (isTarget attrs -> True) "barrage" _ -> do
      let n = environmentCounters attrs
      when (n >= 4) do
        players <- getPlayers
        for_ players \p -> dealIndirectDamage p attrs 2
        removeCounters attrs n
      pure e
    _ -> BadoonShip <$> runMessage msg attrs

-- | "Resolve the Badoon Ship's Charge Up ability": nothing, with it not in play.
chargeUp :: (ReverseQueue m) => m ()
chargeUp = selectOne (EnvironmentIs Cards.badoonShip) >>= traverse_ \ship -> cardSpecific ship "chargeUp" ()
