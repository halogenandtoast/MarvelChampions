{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.FalconWinter.Helpers where

import Marvel.Prelude
import Marvel.Query
import Marvel.Entity
import Marvel.Card
import Marvel.Boost qualified as B
import Data.Maybe (maybeToList)
import Marvel.Attachment.Types hiding (AttachmentId)
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Effect.Types
import Marvel.Event.Types
import Marvel.Game.Source
import Data.HashMap.Strict qualified as HashMap
import Marvel.Trait qualified as T
import Marvel.Status
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref

-- Falcon counts every printed symbol in the boost area, including star symbols.
allBoostIcons :: HasCardDef card => card -> Natural
allBoostIcons = fromIntegral . length . cdBoostIcons . getCardDef

cyberneticArmPaid :: HasGame m => Attrs Event -> m Bool
cyberneticArmPaid attrs = elem PaidWithCyberneticArm <$> getModifiers attrs

-- Safe House searches only the encounter deck. It shuffles even when the search
-- misses; unlike a reveal, putting the minion into play skips When Revealed.
searchEncounterMinion :: ReverseQueue m => IdentityId -> m ()
searchEncounterMinion you = do
  cards <- filter (cardMatch $ CardWithType MinionType) <$> getEncounterDeck
  chooseOrRunOneSearched you (map EncounterCard cards) $
    labeled "$label.done" shuffleEncounterDeck
      : [targeting card (removeFromGame (CardIdRef $ ecCardId card) >> putEncounterCardIntoPlay you card >> shuffleEncounterDeck >> drawCards you 1) | card <- cards]

nextCardReduction :: (ReverseQueue m, IsRef source) => IdentityId -> source -> CardMatcher -> Natural -> m ()
nextCardReduction you source matcher n = when (n > 0) do
  createEffect Cards.fw54019 source (IdentityEntity $ IdentityWithId you)
  cardSpecific source "fwNextCardReduction" (matcher, n)

newtype FwNextCardReductionEffect = FwNextCardReductionEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

fwNextCardReductionEffect :: CardEffect FwNextCardReductionEffect
fwNextCardReductionEffect = effectWith FwNextCardReductionEffect Cards.fw54019 (endsL ?~ DisableAtEndOfPhase)

instance RunMessage FwNextCardReductionEffect where
  runMessage msg e@(FwNextCardReductionEffect attrs) = runQueueT $ case msg of
    CardSpecific target "fwNextCardReduction" (fromJSON -> Success (matcher, n))
      | target == refocus (effectSource attrs), null (effectModifiers attrs) ->
          pure $ FwNextCardReductionEffect attrs {effectModifiers = [ResourceCostReductionFor matcher n]}
    IdentityMessage you (PlayedCard card _) -> do
      valid <- effectValidFor attrs (toRef you)
      when (valid && any (\case ResourceCostReductionFor matcher _ -> cardMatch matcher card; _ -> False) (effectModifiers attrs)) $ disableEffect (effectId attrs)
      pure e
    _ -> FwNextCardReductionEffect <$> runMessage msg attrs

namedCharacters :: HasGame m => [Text] -> m [CharacterId]
namedCharacters names = do
  identities <- concatMapM (selectList . IdentityWithTitle) names
  allies <- concatMapM (selectList . AllyWithTitle) names
  pure $ map IdentityCharacter identities <> map AllyCharacter allies

giveTough :: ReverseQueue m => CharacterId -> m ()
giveTough character = giveStatus character ToughStatus

-- "Find" includes eligible areas in the game, never cards removed from the game.
findPlayerCardToHand :: (ReverseQueue m, IsRef source) => IdentityId -> source -> CardDef -> Natural -> m ()
findPlayerCardToHand you source def damage = do
  owned <- findOwnedPlayerCard you (toCardCode def)
  aside <- getSetAsidePlayerCards
  let found = owned <|> headMay [card | card <- aside, sameCard (getCardDef card) def, isNothing (pcOwner card)]
  for_ found $ \card -> do
    let uid = UpgradeId $ unCardId $ pcCardId card
    wasInPlay <- selectAny $ UpgradeWithId uid
    if wasInPlay
      then do
        modifiers <- getModifiers uid
        unless (CannotLeavePlay `elem` modifiers || CannotLeavePlayByCardAbilities `elem` modifiers) $ do
          returnToHand uid
          chooseDamage you source (toDamage damage FromAbility) AnyEnemy
      else do
        removeFromGame (CardIdRef $ pcCardId card)
        let owner = fromMaybe you $ pcOwner card
        addToHand owner card {pcOwner = Just owner, pcController = Just owner}

findAndActivateMinion :: (ReverseQueue m, IsRef source) => IdentityId -> source -> CardDef -> m ()
findAndActivateMinion you source def = do
  present <- selectOne $ MinionIs def
  case present of
    Just mid -> engageMinion mid you >> cardSpecific source "fwActivateFoundMinion" (you, def)
    Nothing -> do
      searchEverywhereAndRevealEncounterCard you (CardIs def)
      cardSpecific source "fwActivateFoundMinion" (you, def)

attachToNemesisOrVillain :: ReverseQueue m => IdentityId -> Attrs Attachment -> CardDef -> m ()
attachToNemesisOrVillain _ attrs def = do
  nemesis <- selectOne $ MinionIs def
  case nemesis of
    Just mid -> attachTo (attachmentId attrs) mid >> attachedTo mid (attachmentId attrs)
    Nothing -> selectOne ActiveVillain >>= traverse_ (\vid -> attachTo (attachmentId attrs) vid >> attachedTo vid (attachmentId attrs))

activateEnemy :: ReverseQueue m => EnemyId -> IdentityId -> m ()
activateEnemy enemy you = case enemy of
  EnemyAttachmentId _ -> pure ()
  EnemyMinionId mid -> do
    hero <- identityMatches HeroIdentity you
    if hero then enemyAttacks mid you else minionSchemesAgainst mid you
  EnemyVillainId vid -> do
    hero <- identityMatches HeroIdentity you
    if hero then enemyAttacks vid you else villainSchemes vid

techCardsInPlay :: HasGame m => m Natural
techCardsInPlay = countCardsInPlayWithTrait T.Tech

findIce :: ReverseQueue m => CharacterId -> m ()
findIce character = do
  deck <- getEncounterDeck
  discard <- getEncounterDiscard
  case headMay $ filter (cardMatch $ CardIs Cards.fw54035) (deck <> discard) of
    Nothing -> giveStatus character StunnedStatus
    Just card -> do
      you <- getYou
      removeFromGame (CardIdRef $ ecCardId card)
      putEncounterCardIntoPlay you card
      attachToCharacter (AttachmentId $ unCardId $ ecCardId card) character
      shuffleEncounterDeck

attachmentsOnEnemy :: HasGame m => EnemyId -> m [CardDef]
attachmentsOnEnemy enemy = do
  ids <- selectList $ AttachmentOnEnemy enemy
  cards <- findEncounterCards $ CardWithType AttachmentType
  pure [getCardDef card | card <- cards, AttachmentId (unCardId $ ecCardId card) `elem` ids]

iceHost :: Attrs Attachment -> Maybe CharacterId
iceHost attrs = (IdentityCharacter <$> attachmentIdentity attrs)
  <|> (AllyCharacter <$> attachmentAlly attrs)
  <|> (toCharacter <$> attachmentEnemy attrs)

-- RR Move: damage moved to a character is dealt, with its normal prevention windows.
moveIceDamage :: (ReverseQueue m, IsRef source) => source -> CharacterId -> Natural -> m ()
moveIceDamage source host amount = when (amount > 0) $ damageCharacter host source (toDamage amount FromAbility)

hasUpgradeOnCharacter :: HasGame m => CardDef -> CharacterId -> m Bool
hasUpgradeOnCharacter def = \case
  IdentityCharacter you -> selectAny $ UpgradeIs def <> UpgradeOnIdentity (IdentityWithId you)
  AllyCharacter aid -> selectAny $ UpgradeIs def <> UpgradeOnAlly (AllyWithId aid)
  MinionCharacter mid -> enemyMatches (EnemyWithUpgrade $ UpgradeIs def) (EnemyMinionId mid)
  VillainCharacter vid -> enemyMatches (EnemyWithUpgrade $ UpgradeIs def) (EnemyVillainId vid)
  AttachmentCharacter _ -> pure False

readyFriendlyCharacter :: ReverseQueue m => CharacterId -> m ()
readyFriendlyCharacter = \case
  IdentityCharacter you -> ready you
  AllyCharacter aid -> ready aid
  _ -> pure ()

-- The star symbols are not boost icons and are unaffected by a replacement of
-- the boost-icon count. The numerical boost portion uses the shared count window.
countFalconIcons :: (ReverseQueue m, IsRef target, ToJSON payload) => target -> Text -> payload -> [EncounterCard] -> m ()
countFalconIcons target key payload cards = do
  let stars = sum [count (== B.Star) $ cdBoostIcons $ getCardDef card | card <- cards] :: Natural
  countBoostIcons target ("fwCountIcons" :: Text, key, toJSON payload, stars) cards

resolveFalconIcons :: (ReverseQueue m, IsRef target) => target -> Value -> [(EncounterCard, Natural)] -> m ()
resolveFalconIcons target value counts = case fromJSON value of
  Success ("fwCountIcons" :: Text, key :: Text, payload :: Value, stars :: Natural) -> cardSpecific target key (payload, stars + sum (map snd counts))
  _ -> pure ()
