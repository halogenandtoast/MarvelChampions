module Marvel.NextEvolution.Modulars where
import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Keyword
import Marvel.Hp
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Status
import Marvel.Resource.Types
import Marvel.Villain.Types qualified as Villain
import Marvel.Minion.Types qualified as Minion

printedResourcesInHand :: HasGame m => IdentityId -> Resource -> m Natural
printedResourcesInHand you resource = do
  cards <- projectMap PlayerIdentityHand unHand you
  pure $ fromIntegral $ length $ filter (== resource) $ concatMap (printedResources . getCardDef) cards

readyControlledCharacters :: HasGame m => IdentityId -> m [CharacterId]
readyControlledCharacters you = do
  identities <- selectList (IdentityWithId you <> UnexhaustedIdentity)
  allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
  pure $ map IdentityCharacter identities <> map AllyCharacter allies

exhaustFriendlyCharacter :: ReverseQueue m => CharacterId -> m ()
exhaustFriendlyCharacter = \case
  IdentityCharacter you -> exhaust you
  AllyCharacter aid -> exhaust aid
  _ -> pure ()

highestControlled :: HasGame m => IdentityId -> CardType -> m [Target]
highestControlled you typ = do
  targets <- case typ of
    UpgradeType -> map toTarget <$> selectList (UpgradeControlledBy $ IdentityWithId you)
    SupportType -> map toTarget <$> selectList (SupportControlledBy $ IdentityWithId you)
    _ -> pure []
  g <- getGame
  let ranked = [(target, fromMaybe 0 $ cdCost def) | target <- targets, Just def <- [abilityCardDef g target]]
  let highest = case ranked of
        [] -> []
        _ -> [target | (target,cost) <- ranked, cost == maximum (map snd ranked)]
  filterM (\target -> do
    modifiers <- getModifiers target
    let permanent = maybe False (elem Permanent . cdKeywords) (abilityCardDef g target)
    pure $ not permanent && CannotLeavePlay `notElem` modifiers && CannotLeavePlayByCardAbilities `notElem` modifiers) highest

enemiesWithLeastHP :: HasGame m => m [EnemyId]
enemiesWithLeastHP = do
  enemies <- selectList AnyEnemy
  candidates <- for enemies $ \enemy -> (enemy,) <$> getEnemyRemainingHitPoints enemy
  pure $ case candidates of
    [] -> []
    _ -> [enemy | (enemy,hp) <- candidates, hp == minimum (map snd candidates)]

controlledCardsWithCostAtLeast :: HasGame m => IdentityId -> Natural -> m [Target]
controlledCardsWithCostAtLeast you n = do
  allies <- map toTarget <$> selectList (AllyControlledBy $ IdentityWithId you)
  upgrades <- map toTarget <$> selectList (UpgradeControlledBy $ IdentityWithId you)
  supports <- map toTarget <$> selectList (SupportControlledBy $ IdentityWithId you)
  schemes <- map SideSchemeRef <$> getControlledPlayerSideSchemes you False
  g <- getGame
  let candidates = [target | target <- allies <> upgrades <> supports <> schemes, Just def <- [abilityCardDef g target], maybe False (\cost -> toInteger (max 0 cost) >= toInteger n) (cdCost def), Permanent `notElem` cdKeywords def]
  filterM (fmap (\mods -> CannotBeDiscarded `notElem` mods && CannotLeavePlay `notElem` mods && CannotLeavePlayByCardAbilities `notElem` mods) . getModifiers) candidates

-- A partly filled steady status is still a valid target for one more card.
canStunCharacter :: HasGame m => CharacterId -> m Bool
canStunCharacter character = do
  mods <- getModifiers (characterRef character)
  g <- getGame
  count <- getCharacterStatusCount character StunnedStatus
  let printed = maybe [] (toList . cdKeywords) (abilityCardDef g $ characterRef character)
      keywords = printed <> [k | KeywordModifier k <- mods]
      steady = Steady `elem` keywords
      barred = Stalwart `elem` keywords || CannotBeStunned `elem` mods
  pure $ not barred && count < if steady then 2 else 1
