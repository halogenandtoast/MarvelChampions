{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Minion.Minions.AgentsOfShield where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Attachment.Types ()
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hood.Helpers (controlledCharacters, controlledCards)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted hiding (Surge)
import Marvel.Minion.Runner hiding (Surge)
import Marvel.Projection
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype AosMinion = AosMinion (Attrs Minion `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion AosMinion where
  toMinionAttrs (AosMinion (a `With` _)) = a
  overMinionAttrs f (AosMinion (a `With` n)) = AosMinion (f a `With` n)
  minionDisplayCounters (AosMinion (_ `With` n)) = n

aos50072 :: MinionCard AosMinion
aos50072 = minion (AosMinion . (`With` 0)) Cards.aos50072 (Sch 1) (Atk 2) (HP 3)

aos50073 :: MinionCard AosMinion
aos50073 = minion (AosMinion . (`With` 0)) Cards.aos50073 (Sch 1) (Atk 1) (HP 5)

aos50080 :: MinionCard AosMinion
aos50080 = minion (AosMinion . (`With` 0)) Cards.aos50080 (Sch 1) (Atk 2) (HP 4)

aos50083 :: MinionCard AosMinion
aos50083 = minion (AosMinion . (`With` 0)) Cards.aos50083 (Sch 0) (Atk 0) (HP 2)

aos50084 :: MinionCard AosMinion
aos50084 = minion (AosMinion . (`With` 0)) Cards.aos50084 (Sch 1) (Atk 2) (HP 3)

aos50093 :: MinionCard AosMinion
aos50093 = minion (AosMinion . (`With` 0)) Cards.aos50093 (Sch 1) (Atk 2) (HP 3)

aos50094 :: MinionCard AosMinion
aos50094 = minion (AosMinion . (`With` 0)) Cards.aos50094 (Sch 2) (Atk 1) (HP 3)

aos50098 :: MinionCard AosMinion
aos50098 = minion (AosMinion . (`With` 0)) Cards.aos50098 (Sch 1) (Atk 2) (HP 3)

aos50099 :: MinionCard AosMinion
aos50099 = minion (AosMinion . (`With` 0)) Cards.aos50099 (Sch 1) (Atk 3) (HP 4)

aos50100 :: MinionCard AosMinion
aos50100 = minion (AosMinion . (`With` 0)) Cards.aos50100 (Sch 1) (Atk 1) (HP 5)

aos50113 :: MinionCard AosMinion
aos50113 = minion (AosMinion . (`With` 0)) Cards.aos50113 (Sch 1) (Atk 1) (HP 5)

aos50120 :: MinionCard AosMinion
aos50120 = minion (AosMinion . (`With` 0)) Cards.aos50120 (Sch 0) (Atk 2) (HP 4)

aos50125 :: MinionCard AosMinion
aos50125 = minion (AosMinion . (`With` 0)) Cards.aos50125 (Sch 2) (Atk 1) (HP 6)

aos50126 :: MinionCard AosMinion
aos50126 = minion (AosMinion . (`With` 0)) Cards.aos50126 (Sch 3) (Atk 1) (HP 5)

aos50133 :: MinionCard AosMinion
aos50133 = minion (AosMinion . (`With` 0)) Cards.aos50133 (Sch 0) (Atk 1) (HP 5)

aos50139 :: MinionCard AosMinion
aos50139 = minion (AosMinion . (`With` 0)) Cards.aos50139 (Sch 2) (Atk 2) (HP 16)

aos50143 :: MinionCard AosMinion
aos50143 = minion (AosMinion . (`With` 0)) Cards.aos50143 (Sch 0) (Atk 0) (HP 16)

aos50148 :: MinionCard AosMinion
aos50148 = minion (AosMinion . (`With` 0)) Cards.aos50148 (Sch 2) (Atk 1) (HP 14)

aos50152 :: MinionCard AosMinion
aos50152 = minion (AosMinion . (`With` 0)) Cards.aos50152 (Sch 1) (Atk 2) (HP 18)

aos50156 :: MinionCard AosMinion
aos50156 = minion (AosMinion . (`With` 0)) Cards.aos50156 (Sch 1) (Atk 2) (HP 16)

aos50161 :: MinionCard AosMinion
aos50161 = minion (AosMinion . (`With` 0)) Cards.aos50161 (Sch 1) (Atk 2) (HP 16)

aos50172 :: MinionCard AosMinion
aos50172 = minion (AosMinion . (`With` 0)) Cards.aos50172 (Sch 2) (Atk 1) (HP 3)

aos50178 :: MinionCard AosMinion
aos50178 = minion (AosMinion . (`With` 0)) Cards.aos50178 (Sch 0) (Atk 2) (HP 4)

aosReluctantFoeMinion :: MinionCard AosMinion
aosReluctantFoeMinion = minion (AosMinion . (`With` 0)) Cards.aosReluctantFoe (Sch 0) (Atk 0) (HP 1)

aosControlledMinion :: MinionCard AosMinion
aosControlledMinion = minion (AosMinion . (`With` 0)) Cards.aosControlled (Sch 1) (Atk 1) (HP 1)

instance HasModifiersFor AosMinion where
  getModifiersFor _ target (AosMinion (a `With` _)) = case toCardCode a of
    "50083" | target == toRef a -> do
      other <- selectAny (MinionEngagedWith (IdentityWithId $ minionEngagedIdentity a) <> NotMinion (MinionWithId $ minionId a))
      pure [CannotBeAttacked | other]
    code | code `elem` ["50093","50094"], target == toRef a -> do
      high <- selectAny (EnvironmentIs Cards.aos50090b)
      pure [KeywordModifier (if code == "50093" then Surge else Incite 1) | high]
    "50098" | target == toRef a -> pure [KeywordModifier Piercing]
    "50099" | target == toRef a -> pure [KeywordModifier Ranged]
    "50100" | target == toRef a -> pure [AttackModifier $ sum $ map (fromIntegral . max 0 . fromMaybe 0 . cdCost . getCardDef) $ minionHeldCards a]
    "50125" | target == toRef a -> pure [KeywordModifier Piercing, KeywordModifier Ranged]
    "50126" | target == toRef a -> do
      present <- any (cardMatch $ CardIs Cards.aos50125) <$> getVictoryDisplay
      pure [KeywordModifier Villainous | present]
    "50156" -> do
      active <- isJust <$> getEnemyAttack (EnemyMinionId $ minionId a)
      aerial <- case target of
        IdentityRef you -> identityMatches (IdentityWithTrait Trait.Aerial) you
        AllyRef aid -> allyMatches (AllyWithTrait Trait.Aerial) aid
        _ -> pure True
      pure [CannotBasicDefend | active && not aerial]
    _ -> pure []

instance HasAbilities AosMinion where
  getAbilities (AosMinion (a `With` _)) = case toCardCode a of
    "50133" -> [ability a 1 HeroAction NoCriteria ExhaustAnyIdentityCost (runAbility a 1)]
    "50139" -> [windowAbility a 2 (W.EnemyActivates W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 2)]
    "50143" -> [windowAbility a 3 (W.EnemyActivates W.When enemy AnyIdentity) ForcedInterrupt NoCost (runAbility a 3)]
    "50148" -> [windowAbility a 4 (W.MinionHasSchemed W.After $ MinionWithId $ minionId a) ForcedResponse NoCost (runAbility a 4)]
    "50152" -> [windowAbility a 5 (W.EnemyActivates W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 5)]
    "50161" -> [windowAbility a 6 (W.EngagesMinion W.When AnyIdentity $ MinionWithId $ minionId a) ForcedInterrupt NoCost (runAbility a 6)]
    "50172" -> [windowAbility a 7 matcher ForcedInterrupt NoCost (runAbility a 7) | matcher <- [W.EnemyActivates W.When enemy AnyIdentity, W.MinionDefeated W.When (MinionWithId $ minionId a)]]
    _ -> []
   where enemy = EnemyWithId $ EnemyMinionId $ minionId a

instance RunMessage AosMinion where
  runMessage msg e@(AosMinion (a `With` n)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      case toCardCode a of
        "50080" -> do
          found <- selectOne (SideSchemeIs Cards.aos50081)
          when (isNothing found) $ searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.aos50081)
          cardSpecific a "aosAbductAfterSearch" you
        "50084" -> do
          found <- selectList (MinionIs Cards.aos50083)
          if null found then do
            searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.aos50083)
            cardSpecific a "aosScientistEntry" ()
          else do
            traverse_ (\m -> engageMinion m you) found
            placeThreatOnEach a 1 MainScheme
        "50099" -> damageControlled you
        "50120" -> do
          allies <- selectList (AllyWithTrait Trait.Rescued)
          scored <- traverse (\aid -> (aid,) <$> getCharacterRemainingHitPoints (AllyCharacter aid)) allies
          if null scored then do
            cells <- selectList (EnvironmentWithTitle "Holding Cell")
            chooseEffect you [targeting eid (placeCounters eid 1) | eid <- cells]
          else chooseEffect you [targeting aid do
            controller <- fromMaybe you <$> getAllyControllerId aid
            minionStep mid $ MinionAttacksAlly controller aid
            | (aid,hp) <- scored, hp == minimum (map snd scored)]
        "50100" -> do
          weapons <- controlledCards you [UpgradeType] >>= filterM (\t -> maybe False (cardMatch $ CardWithTrait Trait.Weapon) <$> getsGame (`abilityCardDef` t))
          if null weapons then putTopCardsUnder you a 1
          else chooseEffect you [targeting t do
            card <- getsGame (`cardInPlay` t)
            for_ card $ \case PlayerCard c -> removeFromGame t >> placePlayerCardUnder a c; _ -> pure ()
            | t <- weapons]
        _ -> pure ()
      AosMinion . (`With` n) <$> runMessage msg a
    CardSpecific (isTarget a -> True) "mgHoldCard" (fromJSON -> Success card) -> pure $ AosMinion (a {minionHeldCards=minionHeldCards a <> [card]} `With` n)
    CardSpecific (isTarget a -> True) "aosScientistEntry" _ -> do
      absent <- selectNone (MinionIs Cards.aos50083)
      when absent $ placeThreatOnEach a 1 MainScheme
      pure e
    CardSpecific (isTarget a -> True) "aosAbductAfterSearch" (fromJSON -> Success (you :: IdentityId)) -> do
      selectOne (SideSchemeIs Cards.aos50081) >>= traverse_ (\sid -> cardSpecific sid "aosAbductAlly" you)
      pure e
    MinionMessage mid MinionBeginAttack{} | mid == minionId a, toCardCode a == "50099" -> do
      makeAttackIndirect mid
      AosMinion . (`With` n) <$> runMessage msg a
    CardSpecific (isTarget a -> True) "aosPlaceDamageToFive" _ -> do
      hp <- getModifiedHitPoints a
      pure $ AosMinion (a {minionDamage=subtractNatural 5 hp} `With` n)
    MinionMessage mid MinionDefeated | mid == minionId a, toCardCode (minionOriginalCardDef a) == "aosReluctantFoe" -> do
      attached <- selectList (AttachmentIs Cards.aos50171 <> AttachmentOnEnemy (EnemyMinionId mid))
      traverse_ removeFromGame attached
      removeFromGame a
      pure e
    MinionMessage mid MinionDefeated | mid == minionId a -> do
      case toCardCode a of
        "50093" -> alert
        "50094" -> alert
        "50098" -> shuffleMinionIntoEncounterDeck mid
        "50113" -> do
          envs <- selectList AnyEnvironment
          legal <- filterM (fmap (>0) . getCountersOn) envs
          first <- selectJust FirstPlayer
          chooseEffect first [targeting eid (removeCounters eid 1) | eid <- legal]
        "50133" -> placeThreatOnEach a 3 MainScheme
        "50178" -> do
          targets <- controlledCards (minionEngagedIdentity a) [AllyType,SupportType]
          eligible <- filterM (\t -> maybe False (cardMatch $ CardWithTrait Trait.Shield) <$> getsGame (`abilityCardDef` t)) targets
          if null eligible then placeThreatOnEach a 2 MainScheme
          else chooseEffect (minionEngagedIdentity a) [targeting t (removeFromPlay t) | t <- eligible]
        _ -> pure ()
      if toCardCode a == "50098" then pure e else AosMinion . (`With` n) <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let next = n + 1
      when (next >= 3) $ removeFromGame a
      pure $ AosMinion (a `With` next)
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ giveStatus (minionId a) ToughStatus
    RanAbility _ (isTarget a -> True) 3 _ _ -> e <$ dealBoost a
    RanAbility you (isTarget a -> True) 4 _ _ -> do
      attached <- searchAndAttachEncounterCard you (CardIs Cards.aos50149) (minionId a)
      unless attached $ confuseIdentity you
      pure e
    RanAbility _ (isTarget a -> True) 5 windows _ -> do
      for_ [you | W.EnemyActivation _ you <- windows] damageControlled
      pure e
    RanAbility _ (isTarget a -> True) 6 windows _ -> do
      for_ [you | W.EngagedMinion you _ <- windows] (\you -> discardFromHand you 1)
      pure e
    RanAbility _ (isTarget a -> True) 7 _ _ -> e <$ addSecretsToLeast 1
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "50113" -> do
      card <- getBoostCard (toTarget a)
      for_ card $ \c -> enemyActivationAfterCurrent enemy (shuffleIntoEncounterDeck [c])
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "50099" -> do
      getBoostedPlayer enemy >>= damageControlled
      pure e
    _ -> AosMinion . (`With` n) <$> runMessage msg a
   where
    damageControlled you = controlledCharacters you >>= traverse_ (\c -> damageCharacter c a $ toDamage 1 FromAbility)
    alert = selectOne (EnvironmentWithTitle "Alert Level") >>= traverse_ (`placeCounters` 1)
