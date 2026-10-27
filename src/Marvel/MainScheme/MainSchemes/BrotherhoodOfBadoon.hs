-- | The Brotherhood of Badoon's main schemes (The Galaxy's Most Wanted, scenario 1):
-- Terrestrial Invasion (1A/1B) and Protect the Planet (2A/2B).
module Marvel.MainScheme.MainSchemes.BrotherhoodOfBadoon (
  terrestrialInvasion,
  TerrestrialInvasion (..),
  protectThePlanet,
  ProtectThePlanet (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.EncounterCard
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Environment.Environments.BadoonShip (chargeUp)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref

terrestrialInvasion :: MainSchemeCard TerrestrialInvasion
terrestrialInvasion =
  mainScheme TerrestrialInvasion Cards.terrestrialInvasion (PerPlayer 8) (PerPlayer 2) (PerPlayer 2)

newtype TerrestrialInvasion = TerrestrialInvasion (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 1B, First Player Action: exhaust the Milano → remove 3 threat from this scheme.
instance IsMainScheme TerrestrialInvasion where
  mainSchemeAbilities (TerrestrialInvasion a) = [milanoAction a]

instance RunMessage TerrestrialInvasion where
  runMessage msg ms@(TerrestrialInvasion attrs) = runQueueT $ case msg of
    -- 1A, Setup: the Badoon Ship into play; the Milano is already, by its setup keyword
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      ship <- genEncounterCard Environments.badoonShip
      putEncounterCardIntoPlay first ship
      pure ms
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> ms <$ removeThreeThreat attrs
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ nextMainScheme (mainSchemeId attrs)
    _ -> TerrestrialInvasion <$> chargesAfterStepOne msg attrs

protectThePlanet :: MainSchemeCard ProtectThePlanet
protectThePlanet =
  mainScheme ProtectThePlanet Cards.protectThePlanet (PerPlayer 8) (PerPlayer 4) (PerPlayer 2)

newtype ProtectThePlanet = ProtectThePlanet (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | 2B, First Player Action: exhaust the Milano → choose to either remove 3 threat from
-- this scheme or deal 3 damage to a minion.
instance IsMainScheme ProtectThePlanet where
  mainSchemeAbilities (ProtectThePlanet a) = [milanoAction a]

instance RunMessage ProtectThePlanet where
  runMessage msg ms@(ProtectThePlanet attrs) = runQueueT $ case msg of
    -- 2A, When Revealed: Charge Up
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> ms <$ chargeUp
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      -- an option with no minion to damage cannot be chosen (RR Choose)
      minions <- selectAny AnyMinion
      chooseOrRunOne you $
        labeled (countLabel "removeThreatFromThisScheme" 3) (removeThreeThreat attrs)
          : [ labeled (countLabel "dealDamageToAMinion" 3) (chooseDamage you attrs (toDamage 3 FromAbility) MinionEnemy)
            | minions
            ]
      pure ms
    -- completed, the players lose
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    _ -> ProtectThePlanet <$> chargesAfterStepOne msg attrs

milanoAction :: Attrs MainScheme -> Ability
milanoAction a = ability a 1 Action (SelfMatches FirstPlayer) ExhaustMilanoCost $ runAbility a 1

-- | Removed by an encounter card's ability, not a thwart: patrol and the crisis icon do
-- not keep it off the main scheme (RR Patrol, Crisis Icon).
removeThreeThreat :: (ReverseQueue m) => Attrs MainScheme -> m ()
removeThreeThreat attrs = mainSchemeStep (mainSchemeId attrs) (MainSchemeThreatRemoved (toSource attrs) 3)

-- | 1B and 2B, [star] Forced Response: after resolving step one of the villain phase,
-- Charge Up.
chargesAfterStepOne :: (ReverseQueue m) => Message -> Attrs MainScheme -> m (Attrs MainScheme)
chargesAfterStepOne msg attrs = case msg of
  AccelerateMainScheme -> do
    attrs' <- runMessage msg attrs
    forcedResponse (mainSchemeId attrs) chargeUp
    pure attrs'
  _ -> runMessage msg attrs
