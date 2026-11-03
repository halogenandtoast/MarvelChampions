module Marvel.Shadowland.Position where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDamage))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Trait qualified as T

positionCode :: HasGame m => T.Trait -> m (Maybe CardCode)
positionCode trait = do
  position <- selectOne (EnvironmentWithTrait trait)
  case position of
    Nothing -> pure Nothing
    Just eid -> getsGame $ fmap toCardCode . (`cardInPlay` EnvironmentRef eid)

villainAtop :: HasGame m => m Bool
villainAtop = (== Just "65003b") <$> positionCode T.VillainPosition

changePosition :: ReverseQueue m => Bool -> m ()
changePosition once = cardSpecific GameRef "slChangePosition" once

newtype Position = Position (Attrs Environment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsEnvironment
positions :: [EnvironmentCard Position]
positions = map (environment Position) [Cards.street65003a,Cards.street65003b,Cards.street65004a,Cards.street65004b]

instance HasModifiersFor Position where
  getModifiersFor _ target (Position a)
    | toCardCode a `elem` ["65003a","65003b"] = do
      hero <- positionCode T.HeroPosition
      let same = (toCardCode a == "65003a" && hero == Just "65004a") || (toCardCode a == "65003b" && hero == Just "65004b")
      characters <- selectList AnyCharacter
      mainSchemes <- selectList EncounterMainScheme
      pure $ [modifier | target `elem` map characterRef characters, modifier <- if same then [AttackModifier 1] else [AttackDamageReduction 1]]
        <> [ThreatCannotBeRemoved | not same, target `elem` map schemeRef mainSchemes]
    | otherwise = pure []
instance HasAbilities Position where
  getAbilities (Position a)
    | toCardCode a `elem` ["65004a","65004b"] =
      [ (ability a 1 Action first NoCost (runAbility a 1)) {abilityLabel = Just $ if streets then "$label.slMoveByThreat" else "$label.slMoveByDamage"}
      , (ability a 2 Action first (MultiResourceCost [Just resource,Just resource]) (runAbility a 2)) {abilityLabel = Just "$label.slMoveByResources"}
      , (ability a 3 Action first cost (runAbility a 3)) {abilityLabel = Just $ if streets then "$label.slMoveByAerial" else "$label.slMoveByDiscard"}
      ]
    | otherwise = []
   where
    streets = toCardCode a == "65004a"
    first = SelfMatches FirstPlayer
    resource = if streets then Physical else Energy
    cost = if streets then ResourceCost Nothing <> ExhaustCharacterCost (CharacterControlledBy You <> CharacterWithTrait T.Aerial)
      else DiscardControlledCardCost (CardMatchesAny [CardWithType AllyType,CardWithType UpgradeType])
instance RunMessage Position where
  runMessage msg e@(Position a) = runQueueT $ case msg of
    BeginPhase _ -> pure $ Position a {environmentCounters = 0}
    CardSpecific GameRef "slChangePosition" (fromJSON -> Success once) | toCardCode a `elem` ["65003a","65003b"] -> do
      when (not once || environmentCounters a == 0) $ flipEnvironment (environmentId a)
      pure $ Position a {environmentCounters = 1}
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      if toCardCode a == "65004a" then do
        mid <- selectJust EncounterMainScheme
        before <- getSchemeThreatAmount mid
        status <- getCharacterStatusCount (IdentityCharacter you) StunnedStatus
        placeThreatOn a mid 3
        stunIdentity you
        cardSpecific a "checkStreetsMove" (you,mid,before,status)
      else do
        before <- project PlayerIdentityDamage you
        damageIdentity you a (toDamage 3 FromAbility)
        cardSpecific a "checkTenshuMove" (you,before)
      pure e
    CardSpecific (isTarget a -> True) "checkStreetsMove" (fromJSON -> Success (you,mid,before :: Natural,status :: Natural)) -> do
      after <- getSchemeThreatAmount mid
      now <- getCharacterStatusCount (IdentityCharacter you) StunnedStatus
      when (after >= before + 3 && now > status) $ flipEnvironment (environmentId a)
      pure e
    CardSpecific (isTarget a -> True) "checkTenshuMove" (fromJSON -> Success (you,before :: Natural)) -> do
      after <- project PlayerIdentityDamage you
      when (after >= before + 3) $ flipEnvironment (environmentId a)
      pure e
    RanAbility _ (isTarget a -> True) index _ _ | index `elem` [2,3] -> flipEnvironment (environmentId a) >> pure e
    _ -> Position <$> runMessage msg a
