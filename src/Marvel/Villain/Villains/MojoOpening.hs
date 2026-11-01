module Marvel.Villain.Villains.MojoOpening where

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.MojoOpening
import Marvel.Prelude
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype MojoOpeningVillain = MojoOpeningVillain (Attrs Villain)
  deriving anyclass IsVillain
  deriving newtype (Show, Eq, ToJSON, FromJSON)

mojo39001a, mojo39001b, mojo39012a, mojo39012b, mojo39013a, mojo39013b, mojo39014a, mojo39014b :: VillainCard MojoOpeningVillain
mojo39001a = villain MojoOpeningVillain Cards.mojo39001a (Sch 1) (Atk 2) (HP $ PerPlayer 10)
mojo39001b = villain MojoOpeningVillain Cards.mojo39001b (Sch 1) (Atk 3) (HP $ PerPlayer 10)
mojo39012a = villain MojoOpeningVillain Cards.mojo39012a (Sch 1) (Atk 0) (HP $ PerPlayer 13)
mojo39012b = villain MojoOpeningVillain Cards.mojo39012b (Sch 1) (Atk 1) (HP $ PerPlayer 13)
mojo39013a = villainWith MojoOpeningVillain Cards.mojo39013a (Sch 2) (Atk 0) (HP $ PerPlayer 15) (stageL .~ 2)
mojo39013b = villainWith MojoOpeningVillain Cards.mojo39013b (Sch 2) (Atk 2) (HP $ PerPlayer 15) (stageL .~ 2)
mojo39014a = villainWith MojoOpeningVillain Cards.mojo39014a (Sch 2) (Atk 0) (HP $ PerPlayer 17) (stageL .~ 3)
mojo39014b = villainWith MojoOpeningVillain Cards.mojo39014b (Sch 2) (Atk 3) (HP $ PerPlayer 17) (stageL .~ 3)

magog, escaped :: Attrs Villain -> Bool
magog a = toCardCode (getCardDef a) `elem` ["39001a", "39001b"]
escaped a = toCardCode (getCardDef a) `elem` ["39012a", "39013a", "39014a"]

instance HasModifiersFor MojoOpeningVillain where
  getModifiersFor _ target (MojoOpeningVillain a)
    -- MaGog's defeat is replaced by resetting his dial. Suppress the generic
    -- defeat windows; the damage handler below schedules that replacement itself.
    | magog a, target == toRef a = pure [CannotBeDefeated]
    | escaped a, target == toRef a = pure [CannotTakeDamage, CannotBeStunned]
    | escaped a, MainSchemeRef _ <- target = pure [ThreatCannotBeRemoved]
    | otherwise = pure []

instance HasAbilities MojoOpeningVillain where
  getAbilities (MojoOpeningVillain a)
    | magog a = [windowAbility a 1 (W.EnemyAttackedAndDamaged enemy AnyCharacter) ForcedResponse NoCost (runAbility a 1)]
    | escaped a = []
    | otherwise =
        [windowAbility a 2 (W.EnemyActivates W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 2)]
        <> [windowAbility a 3 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 3) | villainStage a == 3]
   where
    enemy = EnemyWithId $ EnemyVillainId $ villainId a

instance RunMessage MojoOpeningVillain where
  runMessage msg@(VillainMessage vid step) (MojoOpeningVillain a)
    | vid == villainId a, magog a, changesDial step = do
        updated <- runMessage msg a
        -- A second lifted handler places the replacement before the generic
        -- after-damage windows, and its returned dial is already reset.
        if unHp (villainHp updated) <= 0
          then runMessage (VillainMessage vid VillainWouldBeDefeated) (MojoOpeningVillain updated)
          else pure $ MojoOpeningVillain updated
   where
    changesDial = \case VillainDamaged _ _ -> True; VillainHitPointsChanged _ -> True; _ -> False
  runMessage msg e@(MojoOpeningVillain a) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId a, magog a -> e <$ villainStep vid VillainWouldBeDefeated
    VillainMessage vid VillainWouldBeDefeated | vid == villainId a, magog a -> do
      players <- fromIntegral <$> getStartingPlayerCount
      rateChallengers ((if toCardCode (getCardDef a) == "39001a" then 3 else 2) * players)
      getPlayers >>= traverse_ dealEncounterCard
      cardSpecific a "magogReset" ()
      pure $ MojoOpeningVillain a {villainHp = HP $ fromIntegral (10 * players)}
    VillainMessage vid step | vid == villainId a, escaped a, Just you <- attackPlayer step -> do
      villainSchemesAgainst vid you
      pure e
    VillainMessage vid FlipVillain | vid == villainId a, not (magog a) -> do
      let other = case toCardCode (getCardDef a) of
            "39012a" -> mojo39012b
            "39012b" -> mojo39012a
            "39013a" -> mojo39013b
            "39013b" -> mojo39013a
            "39014a" -> mojo39014b
            _ -> mojo39014a
      pure $ flipVillainTo other a
    VillainMessage vid VillainDefeated | vid == villainId a, not (magog a) -> do
      difficulty <- getDifficulty
      if villainStage a == 1 || difficulty == Expert && villainStage a == 2
        then do
          advanceVillain vid
          -- The new stage is on the same side as the defeated stage.
          pure $ advanceVillainTo (if villainStage a == 1 then if escaped a then mojo39013a else mojo39013b else if escaped a then mojo39014a else mojo39014b) a
        else MojoOpeningVillain <$> runMessage msg a
    PlaceCounters target n | target == toRef a, not (magog a) -> do
      players <- fromIntegral <$> getStartingPlayerCount
      let total = villainCounters a + n
          threshold = (if villainStage a == 2 then 2 else 3) * players
      if not (escaped a) && total >= threshold
        then flipVillain (villainId a) >> pure (MojoOpeningVillain a {villainCounters = 0})
        else pure $ MojoOpeningVillain a {villainCounters = total}
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ rateChampion (if toCardCode (getCardDef a) == "39001a" then 1 else 2)
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ placeCounters a 1
    RanAbility _ (isTarget a -> True) 3 _ _ -> do
      getPlayers >>= traverse_ (enemyAttacks $ villainId a)
      pure e
    _ -> MojoOpeningVillain <$> runMessage msg a
   where
    attackPlayer = \case
      VillainAttacks you -> Just you
      VillainAttacksWithoutBoost you -> Just you
      VillainAttacksGaining you _ -> Just you
      VillainAttacksWithAdditionalBoostCards you _ -> Just you
      VillainAttacksAlly you _ -> Just you
      _ -> Nothing
