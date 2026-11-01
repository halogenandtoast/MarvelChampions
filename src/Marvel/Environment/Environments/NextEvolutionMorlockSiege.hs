module Marvel.Environment.Environments.NextEvolutionMorlockSiege where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier
import Marvel.Id
import Marvel.Keyword
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype MorlockSiegeEnvironment = MorlockSiegeEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40081a :: EnvironmentCard MorlockSiegeEnvironment
ne40081a = environment MorlockSiegeEnvironment Cards.ne40081a

ne40081b :: EnvironmentCard MorlockSiegeEnvironment
ne40081b = environment MorlockSiegeEnvironment Cards.ne40081b

instance HasModifiersFor MorlockSiegeEnvironment where
  getModifiersFor _ (VillainRef _) (MorlockSiegeEnvironment a)
    | toCardCode a == "40081b", notNull (environmentHeldCards a) =
      pure [KeywordModifier $ Retaliate $ fromIntegral $ length $ environmentHeldCards a]
  getModifiersFor _ _ _ = pure []

instance RunMessage MorlockSiegeEnvironment where
  runMessage msg e@(MorlockSiegeEnvironment a) = runQueueT $ case msg of
    CardSpecific target "neHoldDefeatedVillain" (fromJSON -> Success card) | target == toRef a ->
      pure $ MorlockSiegeEnvironment a {environmentHeldCards = environmentHeldCards a
        <> [MkPlayerCard (ecCardId card) (ecCardDef card) Nothing Nothing]}
    _ -> MorlockSiegeEnvironment <$> runMessage msg a
