module Marvel.AlterEgo.AlterEgos.JamesRhodes (jamesRhodes, JamesRhodes (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Event.Cards qualified as Events
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityDiscard, PlayerIdentityCounters))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Support.Cards qualified as Supports
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

jamesRhodes :: AlterEgoCard JamesRhodes
jamesRhodes = alterEgo JamesRhodes Cards.jamesRhodes (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.equipmentMalfunction]

newtype JamesRhodes = JamesRhodes (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

signatureCards :: CardMatcher
signatureCards = CardMatchesAny $ map CardIs
  [ Allies.ironManWarMachine, Supports.munitionsBunker, Upgrades.upgradedChassis
  , Upgrades.gauntletGun, Upgrades.missileLauncher, Upgrades.shoulderCannon
  , Events.repulsorBeam, Events.targetedStrike, Events.scorchedEarth, Events.fullAuto ]

instance HasAbilities JamesRhodes where
  getAbilities a@(JamesRhodes attrs) =
    [ limitedAbility a 2 (PerPhase 1) Action
        (IsSelf <> ExtendedCardExists (InDiscardOf (IdentityWithId ident) $ BasicCardMatches signatureCards))
        NoCost (runAbility a 2)
    , limitedWindowAbility a 3
        (W.IdentityChangedToForm W.After $ IdentityWithId ident <> AlterEgoIdentity)
        ForcedResponse IsSelf NoCost (runAbility a 3) ]
   where ident = alterEgoIdentityId attrs

instance RunMessage JamesRhodes where
  runMessage msg ae@(JamesRhodes attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 2 _ _ -> do
      cards <- filter (cardMatch signatureCards) <$> projectMap PlayerIdentityDiscard unDiscard ident
      unless (null cards) $ chooseOnePrompted ident "$label.chooseWarMachineCardToShuffle"
        [targeting c (shuffleIntoDeck ident [c]) | c <- cards]
      pure ae
    RanAbility ident (isTarget attrs -> True) 3 _ _ -> do
      counters <- project PlayerIdentityCounters ident
      removeCounters ident counters
      pure ae
    _ -> JamesRhodes <$> runMessage msg attrs
