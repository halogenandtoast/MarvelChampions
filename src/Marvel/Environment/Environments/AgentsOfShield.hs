{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Environment.Environments.AgentsOfShield where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Ally.Types qualified as Ally
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types (Field (MinionDamage))
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype AosEnvironment = AosEnvironment (Attrs Environment)
  deriving anyclass IsEnvironment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

aos50090a :: EnvironmentCard AosEnvironment
aos50090a = environment AosEnvironment Cards.aos50090a

aos50090b :: EnvironmentCard AosEnvironment
aos50090b = environment AosEnvironment Cards.aos50090b

aos50105a :: EnvironmentCard AosEnvironment
aos50105a = environment AosEnvironment Cards.aos50105a

aos50106a :: EnvironmentCard AosEnvironment
aos50106a = environment AosEnvironment Cards.aos50106a

aos50107a :: EnvironmentCard AosEnvironment
aos50107a = environment AosEnvironment Cards.aos50107a

aos50108a :: EnvironmentCard AosEnvironment
aos50108a = environment AosEnvironment Cards.aos50108a

aos50109 :: EnvironmentCard AosEnvironment
aos50109 = environment AosEnvironment Cards.aos50109

aos50110 :: EnvironmentCard AosEnvironment
aos50110 = environment AosEnvironment Cards.aos50110

aos50111 :: EnvironmentCard AosEnvironment
aos50111 = environment AosEnvironment Cards.aos50111

aos50112 :: EnvironmentCard AosEnvironment
aos50112 = environment AosEnvironment Cards.aos50112

aos50131a :: EnvironmentCard AosEnvironment
aos50131a = environment AosEnvironment Cards.aos50131a

aos50131b :: EnvironmentCard AosEnvironment
aos50131b = environment AosEnvironment Cards.aos50131b

aos50181a :: EnvironmentCard AosEnvironment
aos50181a = environment AosEnvironment Cards.aos50181a

aos50182a :: EnvironmentCard AosEnvironment
aos50182a = environment AosEnvironment Cards.aos50182a

aos50183a :: EnvironmentCard AosEnvironment
aos50183a = environment AosEnvironment Cards.aos50183a

instance HasModifiersFor AosEnvironment where
  getModifiersFor _ target (AosEnvironment a) = do
    adaptoid <- case target of
      MinionRef mid -> minionMatches (MinionWithTrait Trait.Adaptoid) mid
      _ -> pure False
    batroc <- case target of VillainRef vid -> enemyMatches (EnemyWithTitle "Batroc") (EnemyVillainId vid); _ -> pure False
    pure $ case toCardCode a of
      "50090b" | batroc -> [SchemeModifier 1,AttackModifier 1]
      "50109" | adaptoid -> [SchemeModifier 1,KeywordModifier (Incite 1),TraitModifier Trait.Aerial]
      "50110" | adaptoid -> [KeywordModifier Villainous,TraitModifier Trait.Psionic]
      "50111" | adaptoid -> [TraitModifier Trait.Elite,AttackModifier 1,KeywordModifier Overkill,KeywordModifier Ranged]
      "50112" | adaptoid -> [AttackModifier 1,KeywordModifier Toughness,TraitModifier Trait.Brute]
      _ -> []

instance HasAbilities AosEnvironment where
  getAbilities (AosEnvironment a) = case toCardCode a of
    code | code `elem` ["50090a","50090b"] -> [ability a 1 HeroAction NoCriteria (ResourceCost Nothing) (runAbility a 1)]
    code | code `elem` ["50105a","50106a","50107a","50108a"] ->
      [ability a 2 HeroAction NoCriteria cost (runAbility a 2) | cost <- [specificCost, MultiResourceCost $ replicate 3 Nothing]]
      where specificCost = if code == "50107a" then ResourceCost (Just Wild)
              else MultiResourceCost $ replicate 2 $ Just $ if code == "50105a" then Energy else if code == "50106a" then Mental else Physical
    "50131b" -> [windowAbility a 3 W.RoundEnds ForcedInterrupt NoCost (runAbility a 3)]
    code | code `elem` ["50181a","50182a","50183a"] ->
      [ability a 4 HeroAction NoCriteria (MultiResourceCost $ replicate 2 $ Just resource) (runAbility a 4) | environmentCounters a > 0]
      where resource = if code == "50181a" then Energy else if code == "50182a" then Mental else Physical
    _ -> []

instance RunMessage AosEnvironment where
  runMessage msg e@(AosEnvironment a) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId a, toCardCode a `elem` ["50105a","50106a","50107a","50108a"] -> do
      amount <- fromIntegral <$> fromGameValue (PerPlayer 2)
      placeCounters a amount
      pure e
    EnvironmentMessage eid (RevealEnvironment _ _) | eid == environmentId a, toCardCode a == "50131a" -> do
      cardSpecific GameRef "aosRevealThunderbolts" eid
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeCounters a 1
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ removeCounters a 1
    RanAbility _ (isTarget a -> True) 3 _ _ -> do
      mids <- selectList (MinionWithTrait Trait.Thunderbolt)
      scored <- traverse (\mid -> (mid,) <$> getMinionDamageInPlay mid) mids
      unless (null scored) do
        first <- selectJust FirstPlayer
        chooseEffect first [targeting mid (cardSpecific GameRef "aosBackupMinion" (environmentId a,mid)) | (mid,d) <- scored, d == maximum (map snd scored)]
      pure e
    RanAbility you (isTarget a -> True) 4 _ _ -> do
      when (environmentCounters a > 0) do
        removeCounters a 1
        case toCardCode a of
          "50181a" -> do
            chars <- selectList (CharacterControlledBy AnyIdentity <> CharacterWithAnyDamage)
            chooseEffect you [targeting (characterRef c) (heal c 1) | c <- chars]
          "50182a" -> removeThreat you a 2 ThwartableScheme
          "50183a" -> do
            enemies <- selectList (EnemyCanTakeDamageFrom $ toSource a)
            chooseEffect you [targeting (enemyRef enemy) (dealDamage enemy a $ toDamage 2 FromAbility) | enemy <- enemies]
          _ -> pure ()
      pure e
    PlaceCounters target _ | isTarget a target -> do
      a' <- runMessage msg a
      checkThreshold a'
      pure $ AosEnvironment a'
    RemoveCounters target amount | isTarget a target -> do
      a' <- runMessage msg a
      when (amount > 0 && environmentCounters a > 0 && environmentCounters a' == 0 && toCardCode a `elem` ["50105a","50106a","50107a","50108a"]) $
        cardSpecific GameRef "aosRescueCell" (environmentId a,toCardCode a)
      pure $ AosEnvironment a'
    CardSpecific (isTarget a -> True) "aosFlipBoardMember" _ -> e <$ flipBoard a
    CardSpecific (isTarget a -> True) "aosRemoveMoreSecrets" (fromJSON -> Success (you,n)) -> e <$ removeSecrets you n
    CheckWindows windows | toCardCode a `elem` ["50090a","50090b"] -> do
      let enemies = [() | W.Window W.When (W.DefeatedMinion _ damage _ _) <- windows, damageSource damage /= FromConsequential]
            <> [() | W.Window W.When (W.DefeatedVillain _ damage _ _) <- windows, damageSource damage /= FromConsequential]
            <> [() | W.Window W.When (W.IdentityDefeatFromDamage _ _ damage) <- windows, damageSource damage /= FromConsequential]
      allies <- filterM (\aid -> not <$> allyDefeatedByConsequential aid) [aid | W.Window W.When (W.DefeatedAlly aid _) <- windows]
      let amount = fromIntegral $ length enemies + length allies
      when (amount > 0) $ placeCounters a amount
      pure e
    _ -> AosEnvironment <$> runMessage msg a
   where
    flipBoard attrs = do
      let def = case toCardCode attrs of "50181a" -> Cards.aos50181b; "50182a" -> Cards.aos50182b; _ -> Cards.aos50183b
          cid = CardId $ unEnvironmentId $ environmentId attrs
          aid = AttachmentId $ unCardId cid
          card = MkEncounterCard cid def
      removeFromGame attrs
      first <- selectJust FirstPlayer
      withTheVillain first (attachEncounterCard card)
      cardSpecific aid "aosSetTransferredCounters" (environmentCounters attrs)
    checkThreshold attrs = case toCardCode attrs of
      "50090a" -> do
        limit <- fromIntegral <$> fromGameValue (PerPlayer 4)
        when (environmentCounters attrs >= limit) do
          removeCounters attrs (environmentCounters attrs)
          environmentStep (environmentId attrs) FlipEnvironment
      "50090b" -> do
        limit <- fromIntegral <$> fromGameValue (PerPlayer 4)
        when (environmentCounters attrs >= limit) $ gameOver Lost
      code | code `elem` ["50181a","50182a","50183a"] -> do
        difficulty <- getDifficulty
        when (environmentCounters attrs >= if difficulty == Expert then 3 else 4) $ flipBoard attrs
      _ -> pure ()
