module Marvel.Environment.Environments.AirStatic where

import Marvel.Id (IdentityId)
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Minion.Minions.Zzzax (controlledEnergy, handEnergy)
import Marvel.Modifier
import Marvel.Phase
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

airStatic :: EnvironmentCard AirStatic
airStatic = environment AirStatic Cards.airStatic

newtype AirStatic = AirStatic (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AirStatic where
  getAbilities (AirStatic attrs) =
    [ windowAbility attrs 1 (PhaseBegins When VillainPhase) ForcedInterrupt NoCost (runAbility attrs 1)
    , (ability attrs 2 HeroAction NoCriteria (DiscardControlledCardCost $ CardWithResource Energy) (runAbility attrs 2)) {abilityLabel = Just "$label.discardEnergyCard"}
    , (ability attrs 3 HeroAction NoCriteria (IndirectDamageCost 2) (runAbility attrs 3)) {abilityLabel = Just "$label.takeTwoIndirectDamage"}
    ]

instance RunMessage AirStatic where
  runMessage msg e@(AirStatic attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      cardSpecific attrs "resolvePlayers" players
      pure e
    CardSpecific (isTarget attrs -> True) "resolvePlayers" (fromJSON -> Success remaining) -> do
      players <- getPlayers
      for_ (headMay players) $ \first -> when (notNull (remaining :: [IdentityId])) $
        chooseOrRunOne first
          [targeting you do
             cardSpecific attrs "forPlayer" you
             cardSpecific attrs "resolvePlayers" (filter (/= you) remaining)
          | you <- remaining, you `elem` players]
      pure e
    CardSpecific (isTarget attrs -> True) "forPlayer" (fromJSON -> Success you) -> do
      n <- (+) <$> handEnergy you <*> controlledEnergy you
      when (n > 0) $ dealIndirectDamage you attrs 2
      pure e
    RanAbility _ (isTarget attrs -> True) idx _ _ | idx `elem` [2, 3] -> e <$ removeFromPlay attrs
    _ -> AirStatic <$> runMessage msg attrs
