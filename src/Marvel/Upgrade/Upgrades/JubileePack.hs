module Marvel.Upgrade.Upgrades.JubileePack where
import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Ability qualified as A
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Game.Source
import Marvel.Entity
import Marvel.Id
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Types
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.Damage

newtype JubileeUpgrade = JubileeUpgrade (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
jubilee47004, jubilee47005, jubilee47013, jubilee47019 :: UpgradeCard JubileeUpgrade
jubilee47004 = upgrade JubileeUpgrade Cards.jubilee47004
jubilee47005 = upgrade JubileeUpgrade Cards.jubilee47005
jubilee47013 = upgrade JubileeUpgrade Cards.jubilee47013
jubilee47019 = upgrade (JubileeUpgrade . (usesL .~ 3) . (discardIfNoUsesL .~ True)) Cards.jubilee47019

instance IsUpgrade JubileeUpgrade where
  upgradeTargets (JubileeUpgrade attrs)
    | toCardCode attrs == "47013" = do
      players <- selectList AnyIdentity
      allowed <- filterM (\p -> selectNone (UpgradeIs Cards.jubilee47013 <> UpgradeOnIdentity (IdentityWithId p))) players
      pure $ Just (map toTarget allowed)
    | otherwise = pure Nothing

instance HasModifiersFor JubileeUpgrade where
  getModifiersFor _ (IdentityRef you) (JubileeUpgrade attrs) | you == upgradeController attrs = do
    jubilee <- identityMatches (IdentityWithTitle "Jubilee" <> HeroIdentity) you
    pure $ if jubilee then case toCardCode attrs of
      "47004" -> [ThwartModifier 1]
      "47005" -> [AttackModifier 1]
      _ -> []
    else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities JubileeUpgrade where
  getAbilities a@(JubileeUpgrade attrs) = case toCardCode attrs of
    "47004" -> [subtype A.Thwart $ limitedWindowAbility a 1 (W.PlaysEvent W.After you $ CardWithTrait T.Thwart) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]
    "47005" -> [subtype A.Attack $ limitedWindowAbility a 1 (W.PlaysEvent W.After you $ CardWithTrait T.Attack) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]
    "47013" -> [subtype A.Thwart $ ability a 1 Action (OwnsThis <> SchemeExists ThwartableScheme) (ExhaustCost <> ExhaustCharacterCost (IdentityCharacters You)) (runAbility a 1)]
    "47019" -> [ability a 1 Action OwnsThis (ExhaustCost <> UseCost) (runAbility a 1)]
    _ -> []
   where you = IdentityWithId $ upgradeController attrs

instance RunMessage JubileeUpgrade where
  runMessage msg a@(JubileeUpgrade attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case toCardCode attrs of
        "47004" -> for_ [eid | W.EventPlayed _ eid _ <- windows] $ \eid -> do
          n <- fromIntegral . length . nub <$> getEventPaidResources eid
          removeThreat you attrs n ThwartableScheme
        "47005" -> for_ [eid | W.EventPlayed _ eid _ <- windows] $ \eid -> do
          n <- fromIntegral . length . nub <$> getEventPaidResources eid
          enemies <- selectList AttackableEnemy
          chooseOne you [targeting (enemyRef enemy) $ dealDamage enemy attrs $ toDamage n $ FromPlayerAttack you | enemy <- enemies]
        "47013" -> removeThreat you attrs 2 ThwartableScheme
        "47019" -> do
          players <- selectList AnyIdentity
          chooseOne you [targeting p $ cardSpecific attrs "cellPhonePlayer" p | p <- players]
        _ -> pure ()
      pure a
    CardSpecific (isTarget attrs -> True) "cellPhonePlayer" (fromJSON -> Success you) -> do
      jubileeCellPhonePower attrs you
      pure a
    _ -> JubileeUpgrade <$> runMessage msg attrs

-- Use the ordinary basic ability, including its exhaustion and status rules.
jubileeCellPhonePower :: ReverseQueue m => Attrs Upgrade -> IdentityId -> m ()
jubileeCellPhonePower attrs you = do
  abilities <- getAbilitiesForMatching you $ \ab -> pure $ abilityType ab == Basic && abilityIndex ab `elem` [300, 301]
  valid <- filterM (\ab -> (&&) <$> passesCriteria you ab <*> canAffordCost you (abilitySource ab) (abilityCost ab)) abilities
  chooseOne you [targeting source do
    createEffect Cards.jubilee47019 attrs (SourceEntity source)
    chooseOne you [pure $ UseAbility ab | ab <- valid, abilitySource ab == source]
    | source <- nub (map abilitySource valid)]

newtype JubileeCellPhoneEffect = JubileeCellPhoneEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

jubileeCellPhoneEffect :: CardEffect JubileeCellPhoneEffect
jubileeCellPhoneEffect = effectWith JubileeCellPhoneEffect Cards.jubilee47019 (modifiersL .~ [ThwartModifier 1, AttackModifier 1])

instance RunMessage JubileeCellPhoneEffect where
  runMessage msg e@(JubileeCellPhoneEffect attrs) = runQueueT $ case msg of
    CheckWindows windows -> do
      let sources = [IdentityRef p | W.Window W.After (W.MadeBasicAttack p) <- windows]
            <> [IdentityRef p | W.Window W.After (W.MadeBasicThwart p) <- windows]
            <> [AllyRef aid | W.Window W.After (W.AllyAttack aid _) <- windows]
            <> [AllyRef aid | W.Window W.After (W.AllyThwart aid _) <- windows]
      when (any ((== effectMatcher attrs) . SourceEntity) sources) $ disableEffect (effectId attrs)
      pure e
    -- Status-replaced ally uses have no completed-power window either. Ally.Runner
    -- consumes the status in the same message, so check it before that use resolves.
    AllyMessage aid AllyAttacked | effectMatcher attrs == SourceEntity (AllyRef aid) -> do
      replaced <- characterHasCondition StunnedStatus (AllyCharacter aid)
      when replaced $ disableEffect (effectId attrs)
      pure e
    AllyMessage aid AllyThwarted | effectMatcher attrs == SourceEntity (AllyRef aid) -> do
      replaced <- characterHasCondition ConfusedStatus (AllyCharacter aid)
      when replaced $ disableEffect (effectId attrs)
      pure e
    RemoveStatus (AllyRef aid) status | effectMatcher attrs == SourceEntity (AllyRef aid), status `elem` [StunnedStatus, ConfusedStatus] -> e <$ disableEffect (effectId attrs)
    -- Stunned/confused basic uses have no completed-power window.
    IdentityMessage p IdentityRemoveStunned | effectMatcher attrs == SourceEntity (IdentityRef p) -> e <$ disableEffect (effectId attrs)
    IdentityMessage p IdentityRemoveConfused | effectMatcher attrs == SourceEntity (IdentityRef p) -> e <$ disableEffect (effectId attrs)
    EndPhase _ -> e <$ disableEffect (effectId attrs)
    _ -> JubileeCellPhoneEffect <$> runMessage msg attrs
