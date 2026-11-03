module Marvel.FearNoEvil.Disasters where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Types
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.SideScheme.Types
import Marvel.Treachery.Types
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype Disaster = Disaster (Attrs Environment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsEnvironment, HasModifiersFor)

disasters :: [EnvironmentCard Disaster]
disasters = [environment (\a -> Disaster a {environmentCounters = 2}) def | def <- [Cards.street60177,Cards.street60178,Cards.street60179]]

instance HasAbilities Disaster where
  getAbilities (Disaster a) =
    [ ability a 1 HeroAction NoCriteria (MultiResourceCost [Just resource,Just resource]) (runAbility a 1)
    , ability a 2 HeroAction NoCriteria (ExhaustCharacterCost $ CharacterControlledBy You) (runAbility a 2)
    ]
   where
    resource = case toCardCode a of "60177" -> Mental; "60178" -> Energy; _ -> Physical

instance RunMessage Disaster where
  runMessage msg e@(Disaster a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> removeCounters a 1 >> pure e
    RanAbility _ (isTarget a -> True) 2 _ payment -> do
      let characters = [c | ExhaustedCharacterPayment c <- payments payment]
      double <- anyM (\c -> case toCardCode a of
        "60177" -> member T.Mystic <$> getCharacterTraits c
        "60178" -> member T.Aerial <$> getCharacterTraits c
        _ -> characterHasCondition ToughStatus c) characters
      removeCounters a (if double then 2 else 1)
      pure e
    RemoveCounters target n | isTarget a target -> do
      when (n >= environmentCounters a) $ removeFromPlay a
      Disaster <$> runMessage msg a
    _ -> Disaster <$> runMessage msg a

newtype CollapsingBridge = CollapsingBridge (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass HasModifiersFor
instance IsSideScheme CollapsingBridge where
  sideSchemeAbilities = getAbilities
collapsingBridge :: SideSchemeCard CollapsingBridge
collapsingBridge = sideScheme CollapsingBridge Cards.street60180 (Static 1)
instance HasAbilities CollapsingBridge where
  getAbilities (CollapsingBridge a) = [windowAbility a 1 (W.ThreatRemovedFromScheme $ SchemeWithId $ SchemeSideSchemeId $ sideSchemeId a) ForcedResponse NoCost (runAbility a 1)]
instance RunMessage CollapsingBridge where
  runMessage msg e@(CollapsingBridge a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> placeThreatOn a (sideSchemeId a) 1 >> pure e
    _ -> CollapsingBridge <$> runMessage msg a

newtype BystandersInPeril = BystandersInPeril (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery
bystandersInPeril :: TreacheryCard BystandersInPeril
bystandersInPeril = treachery BystandersInPeril Cards.street60181
instance RunMessage BystandersInPeril where
  runMessage msg e@(BystandersInPeril a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> addCivilian you True >> pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> getBoostedPlayer enemy >>= \you -> addCivilian you False >> pure e
    _ -> BystandersInPeril <$> runMessage msg a
   where
    addCivilian you search = do
      environments <- selectList (EnvironmentWithTrait T.Disaster)
      if null environments then when search $ searchAndRevealEncounterCard you (CardWithType EnvironmentType <> CardWithTrait T.Disaster)
        else chooseOne you [targeting eid (placeCounters eid 1) | eid <- environments]
