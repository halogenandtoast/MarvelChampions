module Marvel.Hood.Helpers where

import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref

controlledCharacters :: HasGame m => IdentityId -> m [CharacterId]
controlledCharacters you = (<>) <$> (map IdentityCharacter <$> selectList (IdentityWithId you))
  <*> (map AllyCharacter <$> selectList (AllyControlledBy $ IdentityWithId you))

chooseStatus :: ReverseQueue m => IdentityId -> StatusCard -> m ()
chooseStatus you status = do
  chars <- controlledCharacters you
  legal <- filterM (canReceiveStatus status) chars
  chooseOne you [targeting (characterRef c) (giveStatus c status) | c <- legal]
 where
  canReceiveStatus status c = do
    n <- getCharacterStatusCount c status
    mods <- getModifiers (characterRef c)
    def <- getsGame (`abilityCardDef` characterRef c)
    let keywords = maybe [] (toList . cdKeywords) def <> [k | KeywordModifier k <- mods]
        cap = if Steady `elem` keywords && status /= ToughStatus then 2 else 1
    pure $ n < cap && (status == ToughStatus || Stalwart `notElem` keywords)

controlledCards :: HasGame m => IdentityId -> [CardType] -> m [Target]
controlledCards you types = do
  allies <- selectList (AllyControlledBy $ IdentityWithId you)
  supports <- selectList (SupportControlledBy $ IdentityWithId you)
  upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
  let targets = [AllyRef a | AllyType `elem` types, a <- allies]
        <> [SupportRef a | SupportType `elem` types, a <- supports]
        <> [UpgradeRef a | UpgradeType `elem` types, a <- upgrades]
  filterM (\target -> do
    mods <- getModifiers target
    def <- getsGame (`abilityCardDef` target)
    pure $ not (any (`elem` [CannotLeavePlay, CannotBeDiscarded, CannotLeavePlayByCardAbilities]) mods)
      && maybe True (notElem Permanent . cdKeywords) def) targets

discardControlledOfTypes :: ReverseQueue m => IdentityId -> [CardType] -> m Bool
discardControlledOfTypes you types = do
  targets <- controlledCards you types
  chooseOne you [targeting target (removeFromPlay target) | target <- targets]
  pure $ notNull targets

discardControlledCost :: ReverseQueue m => Bool -> IdentityId -> [CardType] -> m Bool
discardControlledCost highest you types = do
  targets <- controlledCards you types
  scored <- fmap catMaybes $ for targets \target -> do
    def <- getsGame (`abilityCardDef` target)
    pure $ (target,) <$> (cdCost =<< def)
  unless (null scored) do
    let extreme = (if highest then maximum else minimum) (map snd scored)
    chooseOne you [targeting target (removeFromPlay target) | (target, cost) <- scored, cost == extreme]
  pure $ notNull scored

discardHandCost :: ReverseQueue m => Bool -> IdentityId -> m ()
discardHandCost highest you = do
  hand <- unHand <$> project PlayerIdentityHand you
  unless (null hand) do
    let cost = fromMaybe 0 . cdCost . getCardDef
        extreme = (if highest then maximum else minimum) (map cost hand)
    chooseOne you [targeting card (discardCard card) | card <- hand, cost card == extreme]

exhaustCharacterChoice :: ReverseQueue m => IdentityId -> m ()
exhaustCharacterChoice you = do
  identities <- selectList (IdentityWithId you <> UnexhaustedIdentity)
  allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
  chooseOne you $ [targeting ident (exhaust ident) | ident <- identities] <> [targeting aid (exhaust aid) | aid <- allies]


activationWillResolve :: HasGame m => IdentityId -> EnemyId -> m Bool
activationWillResolve you enemy = do
  hero <- identityMatches HeroIdentity you
  blocked <- elem CannotActivate <$> getModifiers (enemyRef enemy)
  status <- characterHasCondition (if hero then StunnedStatus else ConfusedStatus) (toCharacter enemy)
  pure $ not (blocked || status)
