module Marvel.NextEvolution.Marauders (marauderAttackChoice, canGainStatus) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ally.Types qualified as Ally ()
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity qualified as Identity ()
import Marvel.Identity.Types ()
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Resource.Types (Resource (Mental))
import Marvel.Status ()

-- | The seven Marauders share their attacks between villain A/B and minion cards.
-- Choices that cannot change the game are not offered. Highest-stat/cost choices
-- determine the highest element BEFORE checking whether its status can be placed.
marauderAttackChoice :: ReverseQueue m => EnemyId -> Text -> Bool -> Bool -> IdentityId -> m ()
marauderAttackChoice enemy name expert isMinion you = case name of
  "Arclight" -> statusOrAttack ConfusedStatus (expert && not isMinion) False 2
  "Blockbuster" -> do
    canGain <- selectAny (CharacterWithId (toCharacter enemy) <> CharacterCanGainTough)
    chooseOne you $ [labeled "$label.neMarauderTough" (enemyBecomesTough enemy) | canGain]
      <> [labeled "$label.neMarauderAttackBonus" do
            changeAttack enemy 2
            when (expert && not isMinion) $ attackGains enemy Overkill]
  "Chimera" -> do
    let cost = MultiResourceCost $ replicate (if expert && not isMinion then 2 else 1) (Just Mental)
    payable <- asYou you $ canAffordCost you (toSource enemy) cost
    chooseOne you $ [labeled "$label.neMarauderMental" (payForTreachery you cost) | payable]
      <> [labeled "$label.neMarauderAttackBonus" (changeAttack enemy 2)]
  "Greycrow" -> do
    allies <- selectList (AllyControlledBy $ IdentityWithId you)
    upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
    supports <- selectList (SupportControlledBy $ IdentityWithId you)
    costed <- for (map AllyRef allies <> map UpgradeRef upgrades <> map SupportRef supports) \target -> do
      def <- getsGame (`abilityCardDef` target)
      pure (target, max 0 $ fromMaybe 0 $ cdCost =<< def)
    let highest = maximum $ 0 : map snd costed
        targets = [target | (target,cost) <- costed, cost == highest]
    discardable <- filterM canDiscard targets
    let discardAction = if expert && not isMinion
          then traverse_ discardControlled discardable
          else chooseOne you [targeting target (discardControlled target) | target <- discardable]
    let choices = [labeled "$label.neMarauderDiscardHighest" discardAction | notNull discardable]
          <> [labeled "$label.neMarauderAttackBonus" (changeAttack enemy highest) | highest > 0]
    unless (null choices) $ chooseOne you choices
  "Harpoon" ->
    chooseOne you
      [labeled "$label.neMarauderIndirect" (dealIndirectDamage you (enemyRef enemy) $ if expert && not isMinion then 3 else 2)
      ,labeled "$label.neMarauderAttackBonus" $
          if isMinion then changeAttack enemy 2 >> attackGains enemy Piercing
          else dealBoost enemy >> when expert (attackGains enemy Overkill)]
  "Riptide" -> chooseOne you
    [labeled "$label.neMarauderThreat" do
        placeThreatOnEach (enemyRef enemy) (if expert && not isMinion then 3 else 2) EncounterMainScheme
        schemes <- selectList AnySideScheme
        traverse_ (`placeThreat` 1) schemes
    ,labeled "$label.neMarauderAttackBonus" do
        changeAttack enemy 2
        when (expert && not isMinion) $ attackGains enemy Ranged >> attackGains enemy Piercing]
  "Vertigo" -> statusOrAttack StunnedStatus (expert && not isMinion) True 2
  _ -> pure ()
 where
  statusOrAttack status onlyHighest useAttack bonus = do
    controlled <- selectList (CharacterControlledBy $ IdentityWithId you)
    powers <- for controlled \character -> (character,) <$> if useAttack
      then characterAttackValue character else characterThwartValue character
    let highest = maximum $ 0 : map snd powers
        candidates = [character | (character,power) <- powers, not onlyHighest || power == highest]
    legal <- filterM (`canGainStatus` status) candidates
    chooseOne you $
      [labeled (if status == StunnedStatus then "$label.neMarauderStun" else "$label.neMarauderConfuse") $
        chooseOne you [targeting (characterRef c) (giveStatus c status) | c <- legal] | notNull legal]
      <> [labeled "$label.neMarauderAttackBonus" (changeAttack enemy bonus)]

canDiscard :: HasGame m => Target -> m Bool
canDiscard target = do
  modifiers <- getModifiers target
  def <- getsGame (`abilityCardDef` target)
  pure $ maybe False (notElem Permanent . cdKeywords) def
    && not (any (`elem` modifiers) [CannotBeDiscarded, CannotLeavePlay, CannotLeavePlayByCardAbilities])

canGainStatus :: HasGame m => CharacterId -> StatusCard -> m Bool
canGainStatus character status = do
  modifiers <- getModifiers (characterRef character)
  def <- getsGame (`abilityCardDef` characterRef character)
  let printed = maybe [] (toList . cdKeywords) def
      keywords = foldr (\modifier ks -> case modifier of KeywordModifier k -> k:ks; LoseKeyword k -> filter (/= k) ks; _ -> ks) printed modifiers
  statusCount <- getCharacterStatusCount character status
  pure $ Stalwart `notElem` keywords
    && (if status == StunnedStatus then CannotBeStunned else CannotBeConfused) `notElem` modifiers
    && statusCount < (if Steady `elem` keywords then 2 else 1)
