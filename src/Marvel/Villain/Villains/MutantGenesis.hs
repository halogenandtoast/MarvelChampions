module Marvel.Villain.Villains.MutantGenesis where

import Data.List qualified as List
import Marvel.Id
import Marvel.Prelude
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Villain.Types
import Marvel.Villain.Cards qualified as Cards
import Marvel.Stats
import Marvel.Hp
import Marvel.Ability
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Damage
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Hand
import Marvel.Deck
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.MainScheme.Types (Field (MainSchemeCounters))
import Marvel.MainScheme.Cards qualified as Schemes
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype MutantGenesisVillain = MutantGenesisVillain (Attrs Villain `With` Maybe (IdentityId, Natural, Natural))
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance IsVillain MutantGenesisVillain where
  toVillainAttrs (MutantGenesisVillain (a `With` _)) = a
  overVillainAttrs f (MutantGenesisVillain (a `With` meta)) = MutantGenesisVillain (f a `With` meta)

mkMutantGenesisVillain :: Attrs Villain -> MutantGenesisVillain
mkMutantGenesisVillain a = MutantGenesisVillain (a `With` Nothing)

mgSabretooth60 :: VillainCard MutantGenesisVillain
mgSabretooth60 = villainWith mkMutantGenesisVillain Cards.mgSabretooth60 (Sch 1) (Atk 2) (HP $ PerPlayer 13) (stageL .~ 1)

mgSabretooth61 :: VillainCard MutantGenesisVillain
mgSabretooth61 = villainWith mkMutantGenesisVillain Cards.mgSabretooth61 (Sch 2) (Atk 2) (HP $ PerPlayer 15) (stageL .~ 2)

mgSabretooth62 :: VillainCard MutantGenesisVillain
mgSabretooth62 = villainWith mkMutantGenesisVillain Cards.mgSabretooth62 (Sch 2) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 3)

mgSentinel84 :: VillainCard MutantGenesisVillain
mgSentinel84 = villainWith mkMutantGenesisVillain Cards.mgSentinel84 (Sch 2) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 1)

mgSentinel85 :: VillainCard MutantGenesisVillain
mgSentinel85 = villainWith mkMutantGenesisVillain Cards.mgSentinel85 (Sch 2) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 2)

mgSentinel86 :: VillainCard MutantGenesisVillain
mgSentinel86 = villainWith mkMutantGenesisVillain Cards.mgSentinel86 (Sch 3) (Atk 3) (HP $ PerPlayer 22) (stageL .~ 3)

mgMasterMold09 :: VillainCard MutantGenesisVillain
mgMasterMold09 = villainWith mkMutantGenesisVillain Cards.mgMasterMold09 (Sch 1) (Atk 2) (HP $ PerPlayer 12) (stageL .~ 1)

mgMasterMold10 :: VillainCard MutantGenesisVillain
mgMasterMold10 = villainWith mkMutantGenesisVillain Cards.mgMasterMold10 (Sch 2) (Atk 3) (HP $ PerPlayer 14) (stageL .~ 2)

mgMasterMold11 :: VillainCard MutantGenesisVillain
mgMasterMold11 = villainWith mkMutantGenesisVillain Cards.mgMasterMold11 (Sch 3) (Atk 4) (HP $ PerPlayer 16) (stageL .~ 3)

mgAvalancheA :: VillainCard MutantGenesisVillain
mgAvalancheA = villain mkMutantGenesisVillain Cards.mgAvalancheA (Sch 2) (Atk 3) (HP $ PerPlayer 15)

mgAvalancheB :: VillainCard MutantGenesisVillain
mgAvalancheB = villain mkMutantGenesisVillain Cards.mgAvalancheB (Sch 2) (Atk 4) (HP $ PerPlayer 18)

mgBlobA :: VillainCard MutantGenesisVillain
mgBlobA = villain mkMutantGenesisVillain Cards.mgBlobA (Sch 1) (Atk 2) (HP $ PerPlayer 16)

mgBlobB :: VillainCard MutantGenesisVillain
mgBlobB = villain mkMutantGenesisVillain Cards.mgBlobB (Sch 2) (Atk 2) (HP $ PerPlayer 19)

mgPyroA :: VillainCard MutantGenesisVillain
mgPyroA = villain mkMutantGenesisVillain Cards.mgPyroA (Sch 2) (Atk 0) (HP $ PerPlayer 14)

mgPyroB :: VillainCard MutantGenesisVillain
mgPyroB = villain mkMutantGenesisVillain Cards.mgPyroB (Sch 2) (Atk 1) (HP $ PerPlayer 17)

mgToadA :: VillainCard MutantGenesisVillain
mgToadA = villain mkMutantGenesisVillain Cards.mgToadA (Sch 2) (Atk 2) (HP $ PerPlayer 13)

mgToadB :: VillainCard MutantGenesisVillain
mgToadB = villain mkMutantGenesisVillain Cards.mgToadB (Sch 3) (Atk 2) (HP $ PerPlayer 16)

mgMagneto38 :: VillainCard MutantGenesisVillain
mgMagneto38 = villainWith mkMutantGenesisVillain Cards.mgMagneto38 (Sch 2) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 1)

mgMagneto39 :: VillainCard MutantGenesisVillain
mgMagneto39 = villainWith mkMutantGenesisVillain Cards.mgMagneto39 (Sch 2) (Atk 3) (HP $ PerPlayer 20) (stageL .~ 2)

mgMagneto40 :: VillainCard MutantGenesisVillain
mgMagneto40 = villainWith mkMutantGenesisVillain Cards.mgMagneto40 (Sch 3) (Atk 3) (HP $ PerPlayer 22) (stageL .~ 3)

instance HasModifiersFor MutantGenesisVillain where
  getModifiersFor _ target (MutantGenesisVillain (a `With` _))
    | target == toRef a, toCardCode (getCardDef a) `elem` ["32109", "32110", "32111"] = pure [SchemesWithoutBoost]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MutantGenesisVillain where
  getAbilities (MutantGenesisVillain (a `With` _)) =
    [windowAbility a 1 (W.EnemyActivates W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 1) | code `elem` ["32060", "32061", "32062"]]
    <> [windowAbility a 2 (W.VillainRevealed W.When (VillainWithId vid) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 2) | code `elem` ["32084", "32085", "32086", "32139", "32140"]]
    <> [windowAbility a 4 (W.EnemyAttacked W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 4) | code `elem` ["32121a", "32121b", "32123a", "32123b", "32138", "32139", "32140"]]
    <> [windowAbility a 5 (W.EnemyAttackedAndDamaged enemy AnyCharacter) ForcedResponse NoCost (runAbility a 5) | code `elem` ["32122a", "32122b", "32124a", "32124b"]]
   where
    vid = villainId a
    enemy = EnemyWithId $ EnemyVillainId vid
    code = toCardCode (getCardDef a)

instance RunMessage MutantGenesisVillain where
  runMessage msg e@(MutantGenesisVillain (attrs `With` pyroState)) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "pyroStart" (fromJSON -> Success you) -> do
      discardFromDeck you 1 attrs
      pure $ MutantGenesisVillain (attrs `With` Just (you, 0, 2))
    WithDiscarded (isTarget attrs -> True) FromDeck cards | Just (you, total, remaining) <- pyroState -> do
      let icons = fromIntegral $ sum $ map (length . cdResources . getCardDef) cards
          sumIcons = total + icons
      if remaining > 1
        then do
          discardFromDeck you 1 attrs
          pure $ MutantGenesisVillain (attrs `With` Just (you, sumIcons, remaining - 1))
        else do
          dealIndirectDamage you attrs sumIcons
          pure $ MutantGenesisVillain (attrs `With` Nothing)
    VillainMessage vid VillainBeginScheme | vid == villainId attrs, toCardCode (getCardDef attrs) `elem` ["32109", "32110", "32111"] -> do
      you <- maybe getActivePlayerId pure (villainActivationPlayer attrs)
      forcedResponse attrs $ discardUntilIntoPlay you (CardWithType MinionType <> CardWithTrait Trait.Sentinel)
      MutantGenesisVillain . (`With` pyroState) <$> runMessage msg attrs
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      let next = List.lookup (toCardCode (getCardDef attrs))
            [("32060", mgSabretooth61), ("32061", mgSabretooth62), ("32084", mgSentinel85), ("32085", mgSentinel86), ("32109", mgMasterMold10), ("32110", mgMasterMold11), ("32138", mgMagneto39), ("32139", mgMagneto40)]
      case next of
        Just card | villainStage attrs == 1 || difficulty == Expert && villainStage attrs == 2 -> do
          advanceVillain vid
          pure $ advanceVillainTo card attrs
        _ -> MutantGenesisVillain . (`With` pyroState) <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs ("heal" :: Text))
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success (key :: Text)) icons | key == "heal" -> e <$ heal (villainId attrs) (sum $ map snd icons)
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      first <- selectJust FirstPlayer
      if toCardCode (getCardDef attrs) `elem` ["32084", "32085", "32086"]
        then do
          searchAndRevealEncounterCard first (CardIs Sides.mgAbductionProtocols)
          when (villainStage attrs > 1) $ filter (/= first) <$> getPlayers >>= traverse_ dealEncounterCard
        else selectList AnyIdentity >>= traverse_ dealEncounterCard
      pure e
    RanAbility you (isTarget attrs -> True) 3 _ _ -> do
      discardUntilIntoPlay you (CardWithType MinionType <> CardWithTrait Trait.Sentinel)
      pure e
    RanAbility you (isTarget attrs -> True) 4 _ _ -> do
      case toCardCode (getCardDef attrs) of
        "32121a" -> exhaustAlly you
        "32121b" -> exhaustAlly you
        "32123a" -> pyro you
        "32123b" -> pyro you
        _ -> selectList EncounterMainScheme >>= traverse_ (\case SchemeMainSchemeId mid -> placeCounters mid 1; SchemeSideSchemeId sid -> placeCounters sid 1)
      pure e
    RanAbility _ (isTarget attrs -> True) 5 [W.EnemyAttacksAndDamages _ character _] _ -> do
      case toCardCode (getCardDef attrs) of
        "32122a" -> giveStatus character StunnedStatus
        "32122b" -> giveStatus character StunnedStatus
        _ -> do
          you <- getSourceController (toSource $ characterRef character)
          for_ you (\p -> discardFrom p RandomFromHand 1)
      pure e
    _ -> MutantGenesisVillain . (`With` pyroState) <$> runMessage msg attrs
   where
    exhaustAlly you = do
      allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
      chooseOrNothing you [targeting ally (exhaust ally) | ally <- allies]
    pyro you = cardSpecific attrs "pyroStart" you

chooseOrNothing :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrNothing you choices = unless (null choices) $ chooseOrRunOne you choices
