module Marvel.Minion.Minions.SmModulars where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Question ()
import Marvel.Status
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Projection
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner

data SmModularsMinionMeta = SmModularsMinionMeta
  { criminalActingPlayer :: Maybe IdentityId
  , criminalWasDefeated :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmModularsMinion = SmModularsMinion (Attrs Minion `With` SmModularsMinionMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion SmModularsMinion where
  toMinionAttrs (SmModularsMinion (attrs `With` _)) = attrs
  overMinionAttrs f (SmModularsMinion (attrs `With` meta)) = SmModularsMinion (f attrs `With` meta)

smModularsMinion :: Attrs Minion -> SmModularsMinion
smModularsMinion attrs = SmModularsMinion (attrs `With` SmModularsMinionMeta Nothing False)

instance HasAbilities SmModularsMinion where
  getAbilities a@(SmModularsMinion (attrs `With` _))
    | toCardCode attrs == "27131" = [ability a 1 AlterEgoAction NoCriteria (ResourceCost $ Just Physical) (runAbility a 1)]
    | toCardCode attrs `elem` ["27158", "27159", "27163"] =
        [windowAbility a 1 (W.EnemyActivates W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) You) ForcedResponse NoCost (runAbility a 1)]
          <> [windowAbility a 2 (W.EngagesMinion W.After You $ MinionWithId $ minionId attrs) ForcedResponse NoCost (runAbility a 2) | toCardCode attrs == "27159"]
    | toCardCode attrs == "27160" =
        [windowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) You) ForcedResponse NoCost (runAbility a 1)]
    | toCardCode attrs `elem` ["27161", "27162"] =
        [windowAbility a 1 (W.EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId attrs) (if toCardCode attrs == "27161" then CharacterControlledBy You else AnyCharacter)) ForcedResponse NoCost (runAbility a 1)]
    | otherwise = []

smRhino :: MinionCard SmModularsMinion
smRhino = minion smModularsMinion Cards.smRhino (Sch 0) (Atk 3) (HP 8)

smCommonCriminal :: MinionCard SmModularsMinion
smCommonCriminal = minion smModularsMinion Cards.smCommonCriminal (Sch 0) (Atk 1) (HP 3)

smLifeSizeDecoy :: MinionCard SmModularsMinion
smLifeSizeDecoy = minion smModularsMinion Cards.smLifeSizeDecoy (Sch 0) (Atk 0) (HP 5)

smEvilDoppelganger :: MinionCard SmModularsMinion
smEvilDoppelganger = minion smModularsMinion Cards.smEvilDoppelganger (Sch 1) (Atk 1) (HP 5)

smDoctorOctopus :: MinionCard SmModularsMinion
smDoctorOctopus = minion smModularsMinion Cards.smDoctorOctopus (Sch 2) (Atk 2) (HP 6)

smElectro :: MinionCard SmModularsMinion
smElectro = minion smModularsMinion Cards.smElectro (Sch 2) (Atk 1) (HP 6)

smHobgoblin :: MinionCard SmModularsMinion
smHobgoblin = minion smModularsMinion Cards.smHobgoblin (Sch 1) (Atk 2) (HP 6)

smKravenTheHunter :: MinionCard SmModularsMinion
smKravenTheHunter = minion smModularsMinion Cards.smKravenTheHunter (Sch 1) (Atk 2) (HP 6)

smScorpion :: MinionCard SmModularsMinion
smScorpion = minion smModularsMinion Cards.smScorpion (Sch 1) (Atk 3) (HP 6)

smVulture :: MinionCard SmModularsMinion
smVulture = minion smModularsMinion Cards.smVulture (Sch 1) (Atk 1) (HP 6)

smEnragedSymbiote :: MinionCard SmModularsMinion
smEnragedSymbiote = minion smModularsMinion Cards.smEnragedSymbiote (Sch 1) (Atk 2) (HP 2)

instance HasModifiersFor SmModularsMinion where
  getModifiersFor _ (MinionRef mid) (SmModularsMinion (attrs `With` _))
    | mid == minionId attrs, toCardCode attrs == "27128" = pure [KeywordModifier Overkill, KeywordModifier Piercing]
    | mid == minionId attrs, toCardCode attrs == "27142" = do
        difficulty <- getDifficulty
        pure [KeywordModifier Toughness | difficulty == Expert]
  getModifiersFor _ (IdentityRef you) (SmModularsMinion (attrs `With` _))
    | toCardCode attrs == "27142", you == minionEngagedIdentity attrs = pure [CannotThwartSideSchemes]

  getModifiersFor _ (MinionRef mid) (SmModularsMinion (attrs `With` _))
    | mid == minionId attrs, toCardCode attrs == "27154" = do
        -- Elimination removes an identity before its minions' queued engagement
        -- changes finish. The final player's defeat leaves no successor at all.
        present <- selectAny (IdentityWithId $ minionEngagedIdentity attrs)
        hand <- if present then projectMap PlayerIdentityHand unHand (minionEngagedIdentity attrs) else pure []
        let n = fromIntegral $ length $ filter (cardMatch HeroSpecificCard) hand
        pure [AttackModifier n, SchemeModifier n]
  getModifiersFor _ _ _ = pure []

instance RunMessage SmModularsMinion where
  runMessage msg m@(SmModularsMinion (attrs `With` meta)) = runQueueT $ case msg of
    CheckWindows windows | toCardCode attrs == "27142"
      , any (\case W.Window W.When (W.MinionEnteredPlay mid) -> mid == minionId attrs; _ -> False) windows -> do
          difficulty <- getDifficulty
          when (difficulty == Expert) $ enemyBecomesTough (minionId attrs)
          pure m
    RanAbility you (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27131" -> do
      dealDamage (minionId attrs) attrs (toDamage 3 FromAbility)
      cardSpecific attrs "criminalReward" ()
      pure $ SmModularsMinion (attrs `With` SmModularsMinionMeta (Just you) False)
    CheckWindows windows | toCardCode attrs == "27131", isJust (criminalActingPlayer meta)
      , any (\case W.Window W.After (W.DefeatedMinion mid _ source _) -> mid == minionId attrs && source == toSource attrs; _ -> False) windows ->
          pure $ SmModularsMinion (attrs `With` meta {criminalWasDefeated = True})
    CardSpecific (isTarget attrs -> True) "criminalReward" _ | Just you <- criminalActingPlayer meta -> do
      when (criminalWasDefeated meta) do
        schemes <- map SchemeSideSchemeId <$> selectList AnySideScheme
        chooseOne you $
          [labeled "$label.smCriminalDraw" (drawCards you 1)]
            <> [labeled "$label.smCriminalRemoveThreat" (chooseOne you [targeting (schemeRef sid) (thwartScheme sid attrs 3) | sid <- schemes]) | not (null schemes)]
      pure $ SmModularsMinion (attrs `With` SmModularsMinionMeta Nothing False)
    RanAbility _ (isTarget attrs -> True) _ _ _ | toCardCode attrs == "27158" -> do
      placeThreatOnEach attrs 1 AnyScheme
      pure m
    RanAbility you (isTarget attrs -> True) _ _ _ | toCardCode attrs == "27159" -> do
      discardFromDeckUntil you (CardMatchesAny [CardWithResource Energy, CardWithResource Wild]) attrs
      pure m
    RanAbility you (isTarget attrs -> True) _ _ _ | toCardCode attrs == "27160" -> do
      dealIndirectDamage you attrs 2
      pure m
    RanAbility you (isTarget attrs -> True) 1 [W.EnemyAttacksAndDamages _ _ _] _ | toCardCode attrs == "27161" -> do
      us <- selectList (UpgradeControlledBy $ IdentityWithId you)
      ss <- selectList (SupportControlledBy $ IdentityWithId you)
      unless (null us && null ss) $ chooseAndDiscardControlled you (map UpgradeRef us <> map SupportRef ss)
      pure m
    RanAbility _ (isTarget attrs -> True) 1 [W.EnemyAttacksAndDamages _ character _] _ | toCardCode attrs == "27162" -> do
      stunned <- characterMatches (CharacterWithStatus StunnedStatus) character
      if stunned then damageCharacter character attrs (toDamage 2 FromAbility) else giveStatus character StunnedStatus
      pure m
    RanAbility you (isTarget attrs -> True) _ _ _ | toCardCode attrs == "27163" -> do
      discardFrom you RandomFromHand 1
      pure m
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27167" -> do
      you <- getBoostedPlayer enemy
      putBoostIntoPlay attrs you
      pure m
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27142" -> do
      you <- getBoostedPlayer enemy
      putBoostIntoPlay attrs you
      pure m
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "27154" -> do
      you <- getBoostedPlayer enemy
      drawCards you 3
      discardFrom you RandomFromHand 3
      pure m
    _ -> SmModularsMinion . (`With` meta) <$> runMessage msg attrs
