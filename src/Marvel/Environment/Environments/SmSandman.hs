module Marvel.Environment.Environments.SmSandman where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Id (EnvironmentId)
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref

newtype SmSandmanEnvironment = SmSandmanEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smCityStreets :: EnvironmentCard SmSandmanEnvironment
smCityStreets = environment SmSandmanEnvironment Cards.smCityStreets

instance HasAbilities SmSandmanEnvironment where
  getAbilities (SmSandmanEnvironment a) =
    [ limited (EachPlayer $ PerRound 1) $
        ability a 1 HeroAction NoCriteria (ExhaustCharacterCost $ CharacterControlledBy You) (runAbility a 1)
    ]

cityStreets :: (HasGame m) => m (Maybe EnvironmentId)
cityStreets = selectOne (EnvironmentIs Cards.smCityStreets)

sandCounters :: (HasGame m) => m Natural
sandCounters = cityStreets >>= maybe (pure 0) getCountersOn

surgingSands :: (ReverseQueue m) => m ()
surgingSands = cityStreets >>= traverse_ (\eid -> cardSpecific eid "surgingSands" ())

surgingSandsFor :: (ReverseQueue m, IsRef target) => target -> m ()
surgingSandsFor target = cityStreets >>= traverse_ (\eid -> cardSpecific eid "surgingSandsFor" $ toTarget target)

instance RunMessage SmSandmanEnvironment where
  runMessage msg e@(SmSandmanEnvironment attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "placeSand" (fromJSON -> Success n) -> e <$ placeCounters attrs (n :: Natural)
    CardSpecific (isTarget attrs -> True) "surgingSands" _ -> do
      placeCounters attrs 1
      cardSpecific attrs "discardSands" ()
      pure e
    CardSpecific (isTarget attrs -> True) "surgingSandsFor" (fromJSON -> Success target) -> do
      placeCounters attrs 1
      cardSpecific attrs "discardSandsFor" (target :: Target)
      pure e
    CardSpecific (isTarget attrs -> True) "discardSands" _ -> e <$ discardTopOfEncounterDeck (environmentCounters attrs)
    CardSpecific (isTarget attrs -> True) "discardSandsFor" (fromJSON -> Success target) ->
      e <$ discardTopOfEncounterDeckFor (environmentCounters attrs) (target :: Target)
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      for_ [character | ExhaustedCharacterPayment character <- payments payment] \character ->
        removeCounters attrs =<< characterAttackValue character
      pure e
    _ -> SmSandmanEnvironment <$> runMessage msg attrs
