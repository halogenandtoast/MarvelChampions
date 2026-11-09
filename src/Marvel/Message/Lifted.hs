-- | What a card, or the engine, does, said as what it does: a handler under 'runQueueT'
-- calls these in the order things happen, and never pushes a message itself. What the
-- game element is given as is any id that names it ('IsEnemy', 'IsCharacter', 'IsScheme', 'IsRef').
--
-- A card imports this in place of "Marvel.Message", "Marvel.Choice" and "Marvel.Queue".
module Marvel.Message.Lifted (
  module Marvel.Message.Lifted,
  module X,
) where

import Marvel.Prelude
import Control.Monad (replicateM_)

import Marvel.Card.Code (CardCode, HasCardCode (toCardCode))

import Marvel.Discard (unDiscard)

import Marvel.Deck (unDeck)

import Marvel.Hand (unHand)

import Marvel.Identity.Types (PlayerIdentity, Field (PlayerIdentityHand, PlayerIdentityDeck, PlayerIdentityDiscard, PlayerIdentityLastDefense, PlayerIdentityTuckedEncounterCards, PlayerIdentityWeatherDeck, PlayerIdentityDamageReduction))

import Marvel.Projection

import Marvel.Ability.Types (Ability)
import Marvel.Ability.Types qualified as Ability
import Marvel.I18n (ikeyWith, intVar)
import Marvel.ActiveCost.Types
import Marvel.Area as X (Area (..), AreaId (..), DeckKey (..), Pile (..))
import Marvel.Attack (Attack (..), sourceEnemy, attackDamageDealt)
import Marvel.Campaign (CampaignRecord (..), CampaignStepKind)
import Marvel.Card.Def (CardDef (..), CardMatcher, HasCardDef (..), CardType (UpgradeType, EventType))
import Marvel.Card.Id (CardId (..), HasCardId (..))
import Marvel.Card.EncounterCard (EncounterCard (..))
import Marvel.Card.PlayerCard.Types (PlayerCard (..), encounterCardInPlayerDeck)
import Marvel.Card.Side (Side (..))
import Marvel.Card.Types (Card (..), IsCard (..))
import Marvel.Cost qualified as Cost
import Marvel.Cost.Types (Cost)
import Marvel.Choice qualified as Choice
import Marvel.Damage
import Marvel.Exception (NoChoices (..))
import Marvel.Game.Source
import Marvel.Game.Source as X (canMoveSchemeThreatFrom)
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Modifier.Types
    ( Modifier(RemovedThreatPlacedOnMinion, CannotLeavePlay, AttacksAgainstGainKeyword, AttacksAgainstLoseKeyword, EnemyAttacksAdditionalCost, AttackAdditionalCostPaid,
               ThwartWithAttack, KeywordModifier, AdditionalReadyCost, AttackDamageReduction,
               CannotChangeEnergyForm, CannotChangeMassForm,
               AttacksCannotBeDefended, OnlyIdentityCanDefend,
               PlayersChooseTheVillain, CannotBeDefeated, CannotBeDiscarded) )
import Marvel.Message as X hiding (cardSpecific, damageIdentity, engageMinion, enemyRetaliates, cancelVillainDefeat, placeThreatOn)
import Marvel.Message qualified as Msg
import Marvel.Message.Lifted.Queue as X
import Marvel.Phase (Phase, VillainPhaseStep)
import Marvel.Question
import Marvel.Queue
import Marvel.Count (selectCount)
import Marvel.Matchers qualified as Matchers
import Marvel.Name (display)
import Marvel.Query (selectAny, selectJust, selectList, selectOne)
import Marvel.Keyword (Keyword (EnergyForm, MassForm, Incite, Permanent, Overkill, Piercing, Ranged, Assault))
import Marvel.Keyword qualified as Keyword
import Marvel.Queue as X (cancelMatchingMessage, replaceMatchingMessage, isQueued)
import Marvel.Ref
import Marvel.Resource.Types (Resource)
import Marvel.Trait.Types (Trait (Weather))
import Marvel.Window.Types (ThreatSource (..), Window (..), WindowTiming (After, When, Would))
import Marvel.Window.Types qualified as W

-- | Storm's separate WEATHER deck is created before her setup choice, without resolving a Special.
setupWeatherDeck :: (ReverseQueue m) => IdentityId -> [CardDef] -> m ()
setupWeatherDeck you defs = do
  cards <- for defs $ \def -> do
    cid <- getRandom
    pure $ MkPlayerCard cid def (Just you) (Just you)
  identityStep you (SetUpWeatherDeck cards)

-- | Swap with a different WEATHER support, then resolve the new support's Special.
swapWeather :: (ReverseQueue m) => IdentityId -> m ()
swapWeather you = do
  cards <- fromMaybe [] <$> project PlayerIdentityWeatherDeck you
  current <- selectOne (SupportMatches [SupportWithTrait Weather, SupportControlledBy $ IdentityWithId you])
  when (isJust current && not (null cards)) $
    chooseOne you [cardLabeled (toCardCode card) $ identityStep you (ChooseWeatherSupport card True) | card <- cards]

swapWeatherSupport :: (ReverseQueue m) => IdentityId -> Maybe SupportId -> PlayerCard -> Bool -> m ()
swapWeatherSupport you old card special = push $ SwapWeatherSupport you old card special

resolveCurrentWeatherSpecial :: (ReverseQueue m) => IdentityId -> m ()
resolveCurrentWeatherSpecial = push . ResolveCurrentWeatherSpecial

-- | A Special that can affect the game waits for its card to be clicked before
-- resolving and opening its after window.
resolveWeatherSpecial :: (ReverseQueue m) => IdentityId -> SupportId -> m ()
resolveWeatherSpecial you sid = whenM (asYou you $ weatherSpecialCanResolve you sid) $ do
  specialAbility you sid $ push $ ResolveWeatherSpecial you sid

-- | The support queues this after its Special's effects, only when it resolves.
finishWeatherSpecial :: (ReverseQueue m) => IdentityId -> SupportId -> m ()
finishWeatherSpecial you sid = push $ WeatherSpecialFinished you sid

-- | Attach an event facedown to a support that permits playing its held events.
attachPlayableEventToSupport :: (ReverseQueue m) => SupportId -> PlayerCard -> m ()
attachPlayableEventToSupport sid card = do
  removeFromGame card
  supportStep sid (AttachPlayableEvent card)

reserveSupportHeldCard :: (ReverseQueue m) => SupportId -> PlayerCard -> m ()
reserveSupportHeldCard sid = supportStep sid . ReserveHeldCard

giveIdentityBoostCards :: (ReverseQueue m) => IdentityId -> Natural -> m ()
giveIdentityBoostCards you n = replicateM_ (fromIntegral n) (dealBoost you)

shuffleTopEncounterCardsIntoPlayerDeck :: (ReverseQueue m) => IdentityId -> Natural -> m ()
shuffleTopEncounterCardsIntoPlayerDeck you n = replicateM_ (fromIntegral n) (push $ PutTopEncounterCardInPlayerDeck you)

-- | A further resolve-mulligans step, before identity setup abilities.
swapIdentityVersion :: (ReverseQueue m) => IdentityId -> CardCode -> CardCode -> m ()
swapIdentityVersion you heroCode alterCode = identityStep you (SwapIdentityVersion heroCode alterCode)

additionalMulligan :: (ReverseQueue m) => IdentityId -> m ()
additionalMulligan you = do
  identityStep you DiscardCards
  identityStep you DrawOrDiscardToHandLimit

addSupportUses :: (ReverseQueue m) => SupportId -> Natural -> m ()
addSupportUses sid = supportStep sid . AddSupportUses

placeSideSchemeAccelerationToken :: (ReverseQueue m) => SideSchemeId -> m ()
placeSideSchemeAccelerationToken sid = sideSchemeStep sid SideSchemePlaceAccelerationToken

showCardsTo :: (ReverseQueue m) => IdentityId -> [Card] -> m ()
showCardsTo you = push . ShowCardsTo you

hideCardsFrom :: (ReverseQueue m) => IdentityId -> m ()
hideCardsFrom = push . HideCardsFrom

consumeBoostCard :: (ReverseQueue m) => EncounterCard -> m ()
consumeBoostCard = push . ConsumeBoostCard

discardBoostToPlayerDiscard :: (ReverseQueue m) => EncounterCard -> IdentityId -> m ()
discardBoostToPlayerDiscard card you = whenM (isCurrentBoostCard card) do
  consumeBoostCard card
  identityStep you (EncounterCardInDiscard card)

putBoostOnBottomOfPlayerDeck :: (ReverseQueue m) => EncounterCard -> IdentityId -> m ()
putBoostOnBottomOfPlayerDeck card you = whenM (isCurrentBoostCard card) do
  consumeBoostCard card
  placeOnBottomOfDeck you (encounterCardInPlayerDeck you card)

putBoostOnTopOfPlayerDeck :: (ReverseQueue m) => EncounterCard -> IdentityId -> m ()
putBoostOnTopOfPlayerDeck card you = whenM (isCurrentBoostCard card) do
  consumeBoostCard card
  placeOnTopOfDeck you (encounterCardInPlayerDeck you card)

dealBoostAsEncounterCard :: (ReverseQueue m, IsRef card) => card -> IdentityId -> m ()
dealBoostAsEncounterCard target you = do
  card <- getBoostCard (toTarget target)
  for_ card \encounter -> do
    consumeBoostCard encounter
    identityStep you (DealtEncounterCard encounter)

-- * Questions
--
-- A question is its choices, each built from what choosing it does: @chooseOne you
-- [targeting enemy (dealDamage enemy attrs damage) | enemy <- enemies]@.

chooseOne :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOne you choices = unless (null choices) $ push . Ask you . ChooseOne =<< sequence choices

-- | Ask one of the choices, saying what is being asked.
chooseOnePrompted :: (ReverseQueue m) => IdentityId -> Text -> [m Choice] -> m ()
chooseOnePrompted you prompt choices = unless (null choices) $ push . Ask you . prompted prompt . ChooseOne =<< sequence choices

-- | Ask one of the choices, saying what is being asked about which card.
chooseOnePromptedAbout :: (ReverseQueue m, IsRef a) => IdentityId -> Text -> a -> [m Choice] -> m ()
chooseOnePromptedAbout you prompt a choices = push . Ask you . promptedAbout prompt a . ChooseOne =<< sequence choices

chooseOneAtATime :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOneAtATime you choices = push . Ask you . ChooseOneAtATime =<< sequence choices

-- | Ask each of the choices in the order the player picks, saying what is being asked.
chooseOneAtATimePrompted :: (ReverseQueue m) => IdentityId -> Text -> [m Choice] -> m ()
chooseOneAtATimePrompted you prompt choices = push . Ask you . prompted prompt . ChooseOneAtATime =<< sequence choices

-- | A forced response of the card, resolved once the active player clicks it, so each
-- player sees what is happening.
forcedResponse :: (ReverseQueue m, IsRef a) => a -> m () -> m ()
forcedResponse a body = do
  you <- getYou
  chooseOnePromptedAbout you "$label.resolveForcedResponse" a [targeting a body]

-- | Resolve a printed Special after its player acknowledges the card. Identity
-- targets use a button so the single-player-choice shortcut cannot answer it.
specialAbility :: (ReverseQueue m, IsRef a) => IdentityId -> a -> m () -> m ()
specialAbility you a body = chooseOnePromptedAbout you "$label.resolveSpecial" a
  [case toTarget a of
    IdentityRef _ -> labeled "$label.resolveSpecial" body
    _ -> targeting a body]

-- | The revealing player resolves the card's When Revealed by clicking it: what follows
-- waits for them.
askToResolveWhenRevealed :: (ReverseQueue m, IsRef a) => IdentityId -> a -> m ()
askToResolveWhenRevealed you a = chooseOnePromptedAbout you "$label.resolveWhenRevealed" a [targeting a (pure ())]

-- | Ask, unless there is only the one choice: that is made. None is an error.
chooseOrRunOne :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrRunOne you = chooseOrRunOneAs you id

-- | 'chooseOrRunOne' among cards searched, all of them shown.
chooseOrRunOneSearched :: (ReverseQueue m) => IdentityId -> [Card] -> [m Choice] -> m ()
chooseOrRunOneSearched you searched = chooseOrRunOneAs you (Searched [(Nothing, searched)])

-- | Ask one of the choices among cards searched in several places, each shown under its
-- label: "from their deck, discard pile or hand".
chooseOneSearchedIn :: (ReverseQueue m) => IdentityId -> Text -> [(Text, [Card])] -> [m Choice] -> m ()
chooseOneSearchedIn you prompt places choices =
  push . Ask you . prompted prompt . Searched [(Just l, cs) | (l, cs) <- places, notNull cs] . ChooseOne =<< sequence choices

chooseOrRunOneAs :: (ReverseQueue m) => IdentityId -> (Question -> Question) -> [m Choice] -> m ()
chooseOrRunOneAs you asked choices = do
  g <- getGame
  -- a choice that would make a permanent card leave play is not one (RR, Permanent)
  filter (offerable g) <$> sequence choices >>= \case
    [] -> throwM NoChoices
    [choice] -> resolveAs you choice
    choices' -> push $ Ask you $ asked $ ChooseOne choices'

-- | A choice offered on the game element, doing what the body does.
targeting :: (ReverseQueue m, IsRef a) => a -> m () -> m Choice
targeting a body = TargetLabel (toTarget a) . pure . Run <$> capture body

-- | A choice by its label (an i18n key under @$label.@), doing what the body does.
labeled :: (ReverseQueue m) => Text -> m () -> m Choice
labeled l body = Label l . pure . Run <$> capture body

-- | A label with a number in it, "Take {count} damage": its key under @label.@ and the
-- count, which also picks the plural form ("Draw a card | Draw {count} cards").
countLabel :: Text -> Natural -> Text
countLabel key n = ikeyWith ("label." <> key) [intVar "count" (fromIntegral n)]

-- | A choice shown as the card, doing what the body does.
cardLabeled :: (ReverseQueue m) => CardCode -> m () -> m Choice
cardLabeled code body = CardLabel code . Run <$> capture body

-- | The card's ability as a choice: what it does is worked out as it is chosen.
targetingAbility :: (Applicative m, IsRef a) => a -> Natural -> m Choice
targetingAbility a n = pure $ TargetLabel (toTarget a) [runAbility a n]

-- | What the choice does, made by "you".
resolve :: (ReverseQueue m) => Choice -> m ()
resolve choice = do
  you <- getYou
  resolveAs you choice

-- | What the choice does, made by the player.
resolveAs :: (ReverseQueue m) => IdentityId -> Choice -> m ()
resolveAs ident choice = pushAll =<< Choice.choiceMessages ident choice

-- | Damage to an enemy matching, the player choosing which.
chooseDamage :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> EnemyMatcher -> m ()
chooseDamage you source damage = resolveAs you . ChooseDamage (toSource source) damage

-- | The player chooses an enemy matching, and the target is told it ('ChoseEnemy').
chooseEnemy :: (ReverseQueue m, IsRef target) => IdentityId -> EnemyMatcher -> target -> m ()
chooseEnemy you matcher = resolveAs you . ChooseEnemy matcher . toTarget

-- | The player chooses an upgrade matching, and the target is told it ('ChoseUpgrade').
chooseUpgrade :: (ReverseQueue m, IsRef target) => IdentityId -> UpgradeMatcher -> target -> m ()
chooseUpgrade you matcher = resolveAs you . ChooseUpgrade matcher . toTarget

-- * Damage and healing

dealDamage :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> Damage -> m ()
dealDamage enemy source = resolve . DamageEnemy (EnemyRef $ toEnemy enemy) (toSource source)

-- | Register a target of an attack-labeled event that deals no damage (for example,
-- Flurry of Blades' confusion). It is still attacked, but damage modifiers cannot turn
-- the status effect into damage.
attackEnemyWithoutDamage :: (ReverseQueue m, IsEnemy enemy, IsRef source) => IdentityId -> enemy -> source -> m ()
attackEnemyWithoutDamage you enemy source = do
  let targetRef = enemyRef enemy
  modifiers <- getModifiers targetRef
  let extra = mconcat [cost | EnemyAttacksAdditionalCost cost <- modifiers]
  if extra /= Cost.NoCost && AttackAdditionalCostPaid (toSource source) `notElem` modifiers
    then cardSpecific GameRef "payEnemyAttackNoDamageCost" (you,toEnemy enemy,toSource source,extra)
    else do
      let target = toEnemy enemy
          from = toSource source
      begun <- sourceHasBegunLabeledAttack from
      targeted <- sourceHasAttackedTarget from target
      modifiers <- (<>) <$> getModifiers (IdentityRef you) <*> getModifiers from
      targetModifiers <- getModifiers (enemyRef target)
      let ranged = KeywordModifier Ranged `elem` modifiers || AttacksAgainstGainKeyword Ranged `elem` targetModifiers
      let windows = [Window When $ W.IdentityAttack you target | not begun] <> [Window When $ W.IdentityAttackTarget you target | not targeted]
      unless (null windows) $ checkWindows windows
      case from of
        EventRef eid -> eventStep eid (EventAttacked you target ranged)
        _ -> do
          unless ranged $ enemyRetaliates target (IdentityCharacter you)
          checkWindows [Window After $ W.IdentityAttack you target]

-- | Initiate the attack before its status effect, then land its damage. Keeping
-- the generated status and damage together allows an attack-target replacement to
-- redirect both while retaining the card's printed order.
confuseThenDealDamage :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> Damage -> m ()
confuseThenDealDamage enemy source damage = do
  messages <- capture $ dealDamage enemy source damage
  let initiates = \case
        CheckWindows windows -> any (\w -> case (windowTiming w, windowType w) of (When, W.IdentityAttack _ _) -> True; (When, W.IdentityAttackTarget _ _) -> True; _ -> False) windows
        _ -> False
  pushAll $ filter initiates messages
  confuse enemy source
  pushAll $ filter (not . initiates) messages

-- | Increase only the pending portion against the selected enemy. Damage against
-- other enemies in the same simultaneous group is unchanged.
addToCurrentAttackAgainst :: ReverseQueue m => IdentityId -> Source -> EnemyId -> Natural -> m ()
addToCurrentAttackAgainst you source enemy n = do
  let matching = \case
        VillainMessage vid (VillainDamaged from d) -> from == source && enemy == EnemyVillainId vid && damageSource d == FromPlayerAttack you
        MinionMessage mid (MinionDamaged from d) -> from == source && enemy == EnemyMinionId mid && damageSource d == FromPlayerAttack you
        _ -> False
      raise = \case
        VillainMessage vid (VillainDamaged from d) -> [VillainMessage vid (VillainDamaged from d {damageAmount = damageAmount d + n})]
        MinionMessage mid (MinionDamaged from d) -> [MinionMessage mid (MinionDamaged from d {damageAmount = damageAmount d + n})]
        msg -> [msg]
  replaceMatchingMessage raise matching

-- | Increase the keyword-bearing portion of the attack whose window is answered.
addToKeywordAttackDamage :: ReverseQueue m => IdentityId -> Natural -> m ()
addToKeywordAttackDamage you n = do
  windows <- getCurrentWindows
  for_ (headMay [(source, enemy, targetSpecific) | Window _ (W.MakingKeywordAttack who source enemy _ targetSpecific) <- windows, who == you]) $ \(source, enemy, targetSpecific) ->
    if targetSpecific then addToCurrentAttackAgainst you source enemy n else modifyCurrentPlayerAttack you n []
  when (any (\case Window _ (W.MakingFriendlyKeywordAttack who _ _ _) -> who == you; _ -> False) windows) $
    modifyCurrentPlayerAttack you n []


-- | Increase only the ranged attack whose interrupt is being answered.
addToRangedAttackDamage :: ReverseQueue m => IdentityId -> Natural -> m ()
addToRangedAttackDamage you n = do
  windows <- getCurrentWindows
  when (any (\case Window _ (W.MakingFriendlyKeywordAttack who _ _ keywords) -> who == you && Ranged `elem` keywords; _ -> False) windows) $
    modifyCurrentPlayerAttack you n []
  for_ (headMay [(source, enemy) | Window _ (W.MakingRangedAttack who source enemy) <- windows, who == you]) $ \(source, enemy) -> do
    let increase damage = damage {damageAmount = damageAmount damage + n}
        matching = \case
          VillainMessage vid (VillainDamaged from _) -> from == source && enemy == EnemyVillainId vid
          MinionMessage mid (MinionDamaged from _) -> from == source && enemy == EnemyMinionId mid
          _ -> False
        raise = \case
          VillainMessage vid (VillainDamaged from damage) -> [VillainMessage vid (VillainDamaged from $ increase damage)]
          MinionMessage mid (MinionDamaged from damage) -> [MinionMessage mid (MinionDamaged from $ increase damage)]
          msg -> [msg]
    replaceMatchingMessage raise matching
    replaceMatchingMessage (\case CheckWindows ws -> [CheckWindows $ map (\w -> w {W.windowType = case W.windowType w of
                                              W.DamagedVillain vid damage | enemy == EnemyVillainId vid -> W.DamagedVillain vid $ increase damage
                                              W.VillainDamageFrom vid from damage | enemy == EnemyVillainId vid && from == source -> W.VillainDamageFrom vid from $ increase damage
                                              original -> original}) ws]; msg -> [msg]) $ \case
      CheckWindows ws -> any (\w -> case W.windowType w of W.VillainDamageFrom vid from _ -> enemy == EnemyVillainId vid && from == source; _ -> False) ws
      _ -> False

-- | All characters take their damage before any after-damage responses.
damageCharacters :: (ReverseQueue m, IsRef source) => source -> Damage -> [CharacterId] -> m ()
damageCharacters source damage characters = pushAll . simultaneousDamage =<< traverse (\c -> capture $ damageCharacter c source damage) characters

-- | Damage dealt to each of the enemies as one effect ('simultaneousDamage').
dealDamageToEach :: (ReverseQueue m, IsEnemy enemy, IsRef source) => [enemy] -> source -> Damage -> m ()
dealDamageToEach enemies source damage =
  pushAll . simultaneousDamage =<< traverse (\enemy -> capture $ dealDamage enemy source damage) enemies

-- | Damage dealt to each of the enemies, each its own amount, as one effect ('simultaneousDamage').
dealDamagesToEach :: (ReverseQueue m, IsEnemy enemy, IsRef source) => source -> [(enemy, Damage)] -> m ()
dealDamagesToEach source dealt = pushAll . simultaneousDamage =<< traverse (\(enemy, damage) -> capture $ dealDamage enemy source damage) dealt

-- | Deal the damage, then what the body does: before anything that answers the damage,
-- as the window after the attack or retaliate.
dealDamageThen :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> Damage -> m () -> m ()
dealDamageThen enemy source damage body = do
  (before, now, after) <- splitDamage <$> capture (dealDamage enemy source damage)
  rest <- capture body
  pushAll $ before <> now <> rest <> after

-- | Deal the damage and what the body does as one effect: "deal 2 damage and stun it".
dealDamageAnd :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> Damage -> m () -> m ()
dealDamageAnd enemy source damage body = do
  (before, now, after) <- splitDamage <$> capture (dealDamage enemy source damage)
  also <- capture body
  pushAll $ before <> [Simultaneously (also <> now)] <> after

-- | Damage to each enemy matching.
damageAllEnemies :: (ReverseQueue m, IsRef source) => source -> Damage -> EnemyMatcher -> m ()
damageAllEnemies source damage matcher = resolve $ DamageAllEnemies matcher (toSource source) damage

-- | Damage to each character matching, as one effect.
damageAllCharacters :: (ReverseQueue m, IsRef source) => source -> Damage -> CharacterMatcher -> m ()
damageAllCharacters source damage matcher = resolve $ DamageAllCharacters matcher (toSource source) damage

-- | Damage to the character: an identity's goes through 'damageIdentity'; an enemy's
-- is taken as it is, without the window and bonuses of a player's 'dealDamage'.
damageCharacter :: (ReverseQueue m, IsCharacter character, IsRef source) => character -> source -> Damage -> m ()
damageCharacter character source = resolve . DamageCharacter (toCharacter character) (toSource source)

-- | An attack's damage to the identity, which is not 'damageIdentity''s.
attackDamageIdentity :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> m ()
attackDamageIdentity ident source = push . IdentityMessage ident . IdentityDamaged (toSource source)

-- | A friendly character really attacks, for attack-triggered abilities and ally
-- consequential damage. This is not an enemy activation or a basic attack.
friendlyCharacterAttacks :: (ReverseQueue m, IsCharacter character) => character -> IdentityId -> m ()
friendlyCharacterAttacks attacker = push . FriendlyCharacterAttacks (toCharacter attacker)

-- | The window after an enemy's attack damages the character, if the damage is that.
attackDealtDamageTo :: (ReverseQueue m, IsCharacter character) => Source -> Damage -> character -> m ()
attackDealtDamageTo source damage = pushAll . attackDealtDamage source damage . toCharacter

heal :: (ReverseQueue m, IsCharacter character) => character -> Natural -> m ()
heal character = resolve . Heal (toCharacter character)

-- | Damage to the hero or alter-ego not from an attack, the "would take damage" window
-- first ('Msg.damageIdentity').
damageIdentity :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> m ()
damageIdentity ident source = pushAll . Msg.damageIdentity ident (toSource source)

-- | The retaliate damage dealt to the character that attacked.
retaliate :: (ReverseQueue m, IsRef source) => source -> CharacterId -> Natural -> m ()
retaliate source attacker = pushAll . retaliateDamage (toSource source) attacker

-- | Indirect damage dealt to the player: they divide it among the characters they
-- control, each point through 'damageCharacter' (an identity's 'damageIdentity').
dealIndirectDamage :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> m ()
dealIndirectDamage ident source n = indirectDamageAssigned ident (toSource source) (toDamage n FromAbility) []

-- | An attack's damage dealt as indirect damage (Starshark): divided as 'dealIndirectDamage'
-- divides it, each share the attack's damage.
dealIndirectAttackDamage :: (ReverseQueue m) => IdentityId -> Source -> Damage -> m ()
dealIndirectAttackDamage ident source damage = indirectDamageAssigned ident source damage []

-- | Indirect damage dealt among the players: the first player divides it among the
-- friendly characters in play.
dealIndirectDamageAmongPlayers :: (ReverseQueue m, IsRef source) => source -> Natural -> m ()
dealIndirectDamageAmongPlayers source n = push $ AssignIndirectDamageAmongPlayers (toSource source) n []

-- | Damage divided among the enemies matching as the player chooses, assigned a point
-- at a time, then each enemy given a share dealt it as one effect: an attack's damage
-- attacks each of them (Giant Wasp, Wasp Sting). The damage is the total and its kind.
dealDividedDamage :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> EnemyMatcher -> m ()
dealDividedDamage you source damage matcher = push $ AssignDividedDamage you (toSource source) damage matcher []

-- | Choose both divided allocations before resolving their damage and thwart as one effect.
dealDividedDamageAndThwart :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> SchemeMatcher -> EnemyMatcher -> m ()
dealDividedDamageAndThwart you source damage schemes enemies = push $ DivideDamageAndThreat you (toSource source) damage schemes enemies

-- | The indirect damage left to assign, and what has been.
indirectDamageAssigned :: (ReverseQueue m) => IdentityId -> Source -> Damage -> [(CharacterId, Natural)] -> m ()
indirectDamageAssigned ident source n = push . AssignIndirectDamage ident source n

setHitPoints :: (ReverseQueue m) => IdentityId -> Natural -> m ()
setHitPoints ident = push . IdentityMessage ident . IdentitySetHitPoints

-- * Threat

thwartScheme :: (ReverseQueue m, IsScheme scheme, IsRef source) => scheme -> source -> Natural -> m ()
thwartScheme scheme source = resolve . ThwartScheme (SchemeRef $ toScheme scheme) (toSource source)

-- | Threat removed from each of the schemes, each its own amount, as one effect: a scheme
-- left with none is defeated once all of it is removed.
thwartEach :: (ReverseQueue m, IsScheme scheme, IsRef source) => source -> [(scheme, Natural)] -> m ()
thwartEach source removed = do
  msgs <- concat <$> traverse (\(scheme, n) -> capture $ thwartScheme scheme source n) removed
  unless (null msgs) $ push (Simultaneously msgs)

thwartEachAs :: (ReverseQueue m, IsScheme scheme, IsRef source) => IdentityId -> source -> [(scheme, Natural)] -> m ()
thwartEachAs you source removed = do
  msgs <- concat <$> traverse (\(scheme, n) -> capture $ thwartSchemeAs you scheme source n) removed
  unless (null msgs) $ push (Simultaneously msgs)

-- | Choose targets first, then remove threat and apply the accompanying effect together.
thwartSchemeAnd :: (ReverseQueue m, IsScheme scheme, IsRef source) => IdentityId -> scheme -> source -> Natural -> m () -> m ()
thwartSchemeAnd you scheme source n body = do
  messages <- capture $ thwartSchemeAs you scheme source n >> body
  unless (null messages) $ push (Simultaneously messages)

-- | A hero's or ally's basic thwart of the scheme.
basicThwartOf :: (ReverseQueue m, IsScheme scheme, IsRef source) => scheme -> source -> Natural -> m ()
basicThwartOf scheme source = resolve . BasicThwartScheme (SchemeRef $ toScheme scheme) (toSource source)

-- | The threat placed on the scheme without the "would place threat" window
-- ('threatPlacement'): what an interrupt to that window places in its stead.
placeThreatWithoutWindow :: (ReverseQueue m, IsScheme scheme) => scheme -> Natural -> m ()
placeThreatWithoutWindow scheme = push . threatPlacement (toScheme scheme)

-- | A prescribed scheme is thwarted by the given player, without a target question.
thwartSchemeAs :: (ReverseQueue m, IsScheme scheme, IsRef source) => IdentityId -> scheme -> source -> Natural -> m ()
thwartSchemeAs you scheme source n = resolveAs you $ ThwartScheme (SchemeRef $ toScheme scheme) (toSource source) n

-- | Threat removed by thwarting, divided among the schemes matching as the player
-- chooses: assigned a point at a time, none past a scheme's threat, then each scheme
-- given its share is thwarted (RR, Wasp rulesheet).
thwartDivided :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> SchemeMatcher -> m ()
thwartDivided you source n matcher = prepareDividedThreat you (toSource source) False n matcher

-- | 'thwartDivided', "a total of up to" n: the player may stop short.
thwartDividedUpTo :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> SchemeMatcher -> m ()
thwartDividedUpTo you source n matcher = prepareDividedThreat you (toSource source) True n matcher

-- | The first step of a labeled divided thwart opens its interrupt before
-- calculating bonuses in the shared total.
prepareDividedThreat :: ReverseQueue m => IdentityId -> Source -> Bool -> Natural -> SchemeMatcher -> m ()
prepareDividedThreat you source upTo n matcher = do
  controller <- getHeroSourceController source
  labeled <- elem Ability.Thwart <$> sourceAbilitySubTypes source
  begun <- sourceHasBegunLabeledThwart source
  when (labeled && not begun) do
    cardSpecific GameRef "markLabeledThwart" source
    case source of
      EventRef eid -> eventStep eid EventThwarted
      _ -> pure ()
    for_ controller $ \ident -> checkWindows [Window When $ W.IdentityThwart ident source]
  push $ PrepareDividedThreat you source upTo n matcher

-- | The hero's basic thwart divided among the thwartable schemes (Giant Wasp).
basicThwartDivided :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> Natural -> m ()
basicThwartDivided you source thw atk = do
  schemes <- selectList (SchemeAHeroCanBasicThwart you)
  normal <- getCharacterThwart (IdentityCharacter you)
  choices <- for schemes $ \scheme -> do
    assault <- schemeHasKeyword scheme Assault
    withAttack <- elem ThwartWithAttack <$> getModifiers (schemeRef scheme)
    conditional <- getCharacterThwartAgainstScheme (IdentityCharacter you) scheme
    let extra = subtractNatural normal conditional
        assign usingAttack amount = do
          let granted = if usingAttack then 0 else extra
              pool = amount + granted
          when (pool > 0) $ cardSpecific GameRef "assignBasicThwartWithStat"
            (you, toSource source, usingAttack, normal, pool - 1, [(scheme, 1 :: Natural)], granted)
    pure $ targeting (schemeRef scheme) $
      if assault then assign True atk else if withAttack && atk /= thw + extra
        then chooseOne you
          [ labeled "$label.thwartWithThw" (assign False thw)
          , labeled "$label.thwartWithAtk" (assign True atk)
          ]
        else assign False thw
  case choices of
    [_] -> chooseOrRunOne you choices
    _ -> chooseOnePrompted you "$label.assignDividedThreat" choices

-- | Add threat removal to the thwart whose interrupt window is open.
increaseCurrentThwart :: (HasQueue m) => Source -> SchemeId -> Natural -> m ()
increaseCurrentThwart source scheme bonus = replaceMatchingMessage increase matches
 where
  matches = \case
    CardSpecific target "resolveThwart" (fromJSON -> Success (from, _ :: Natural)) -> target == schemeRef scheme && from == source
    _ -> False
  increase = \case
    CardSpecific target "resolveThwart" (fromJSON -> Success (from :: Source, n :: Natural)) -> [CardSpecific target "resolveThwart" $ toJSON (from, n + bonus)]
    msg -> [msg]

-- | Remove threat without thwarting the scheme.
removeSchemeThreat :: (ReverseQueue m, IsRef source) => source -> SchemeId -> Natural -> m ()
removeSchemeThreat source scheme n = push $ case scheme of
  SchemeMainSchemeId mid -> MainSchemeMessage mid (MainSchemeThreatRemoved (toSource source) n)
  SchemeSideSchemeId sid -> SideSchemeMessage sid (SideSchemeThreatRemoved (toSource source) n)

-- | Threat removed from a scheme matching, the player choosing which.
removeThreat :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> SchemeMatcher -> m ()
removeThreat you source n = resolveAs you . RemoveThreat (toSource source) n

-- | Threat a card places on the scheme, the "would place threat" window first
-- ('placeThreatOn').
placeThreat :: (ReverseQueue m, IsScheme scheme) => scheme -> Natural -> m ()
placeThreat = placeThreatFrom ThreatFromAbility

placeThreatOn :: (ReverseQueue m, IsRef source, IsScheme scheme) => source -> scheme -> Natural -> m ()
placeThreatOn source scheme n = placeThreatBy source ThreatFromAbility scheme n

-- | Retain the acting source until character replacement interrupts have resolved.
placeThreatBy :: (ReverseQueue m, IsRef source, IsScheme scheme) => source -> ThreatSource -> scheme -> Natural -> m ()
placeThreatBy source from (toScheme -> scheme) n =
  cardSpecific GameRef "placeThreatBy" (toSource source,from,scheme,n)

-- | Report positive actual placements to the source's "threatPlaced" step.
-- A canceled or fully prevented placement reports nothing.
placeThreatOnReporting :: (ReverseQueue m, IsRef source, IsScheme scheme) => source -> scheme -> Natural -> m ()
placeThreatOnReporting source (toScheme -> scheme) n = when (n > 0) $
  cardSpecific GameRef "placeReportingThreatBy" (toSource source,ThreatFromAbility,scheme,n)

placeThreatOnReportingAfterCharacterWindow :: ReverseQueue m => Source -> SchemeId -> Natural -> m ()
placeThreatOnReportingAfterCharacterWindow source scheme n = when (n > 0) $
  pushAll [CheckWindows [Window Would $ W.ThreatPlaced ThreatFromAbility scheme n],WithThreatPlacementResult (toSource source) $ threatPlacement scheme n]

-- | Threat placed on the scheme by what the window says, the window first.
placeThreatFrom :: (ReverseQueue m, IsScheme scheme) => ThreatSource -> scheme -> Natural -> m ()
placeThreatFrom from (toScheme -> scheme) n =
  pushAll $ [CheckWindows [Window Would $ W.ThreatPlaced from scheme n] | n > 0] <> [threatPlacement scheme n]

-- | Threat placed on each scheme matching.
placeThreatOnEach :: (ReverseQueue m, IsRef source) => source -> Natural -> SchemeMatcher -> m ()
placeThreatOnEach source n = resolve . PlaceThreat (toSource source) n

-- | Incite X, as the card is revealed: X threat on the main scheme, each of them where
-- there are two (Tower Defense, MTS rulebook p.10).
incite :: (ReverseQueue m, HasCardDef card, IsRef card) => card -> m ()
incite card = do
  modifiers <- getModifiers card
  let keywords = toList (cdKeywords $ getCardDef card) <> [k | KeywordModifier k <- modifiers]
  for_ [n | Incite n <- keywords] $ \n -> do
    schemes <- selectList EncounterMainScheme
    for_ schemes (`placeThreat` n)

-- * Status cards and counters

stun :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> m ()
stun enemy = resolve . Stun (EnemyRef $ toEnemy enemy) . toSource

confuse :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> m ()
confuse enemy = resolve . Confuse (EnemyRef $ toEnemy enemy) . toSource

stunIdentity :: (ReverseQueue m) => IdentityId -> m ()
stunIdentity ident = push $ IdentityMessage ident IdentityStunned

giveStatus :: (ReverseQueue m, IsCharacter character) => character -> StatusCard -> m ()
giveStatus character = push . GiveStatus (characterRef $ toCharacter character)

removeStatus :: (ReverseQueue m, IsCharacter character) => character -> StatusCard -> m ()
removeStatus character = push . RemoveStatus (characterRef $ toCharacter character)

-- | The identity is given a tough status card.
becomeTough :: (ReverseQueue m) => IdentityId -> m ()
becomeTough ident = push $ IdentityMessage ident IdentityBecomeTough

-- | The identity's stun is removed, in place of what it would do.
removeStun :: (ReverseQueue m) => IdentityId -> m ()
removeStun ident = push $ IdentityMessage ident IdentityRemoveStunned

removeConfused :: (ReverseQueue m) => IdentityId -> m ()
removeConfused ident = push $ IdentityMessage ident IdentityRemoveConfused

confuseIdentity :: (ReverseQueue m) => IdentityId -> m ()
confuseIdentity ident = push $ IdentityMessage ident IdentityConfused

stunAlly :: (ReverseQueue m) => AllyId -> m ()
stunAlly ally = push $ AllyMessage ally AllyStunned

-- | The enemy is given a tough status card.
enemyBecomesTough :: (ReverseQueue m, IsEnemy enemy) => enemy -> m ()
enemyBecomesTough enemy = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid VillainBecomeTough
  EnemyMinionId mid -> MinionMessage mid MinionBecomeTough
  EnemyAttachmentId _ -> CheckWindows []

-- | Counters placed on, or removed from, an environment, main scheme, attachment or
-- identity card.
placeCounters :: (ReverseQueue m, IsRef a) => a -> Natural -> m ()
placeCounters a = push . PlaceCounters (counterTarget a)

removeCounters :: (ReverseQueue m, IsRef a) => a -> Natural -> m ()
removeCounters a = push . RemoveCounters (counterTarget a)

-- | Growth counters placed on Groot, "to a maximum of 10".
placeGrowthCounters :: (ReverseQueue m) => IdentityId -> Natural -> m ()
placeGrowthCounters ident n = push $ IdentityMessage ident (PlaceCountersUpTo n 10)

-- * Players and allies

-- | More DEF for the hero's defense under way.
addDefense :: (ReverseQueue m) => IdentityId -> Natural -> m ()
addDefense ident = push . IdentityMessage ident . IdentityDefended

-- | What can be readied.
class Readyable a where
  readied :: a -> Message
  -- | readied by the game or an encounter card, not a player card's ability
  readiedByGame :: a -> Message
  readiedByGame = readied

instance Readyable IdentityId where
  readied ident = IdentityMessage ident ReadiedIdentity
  readiedByGame ident = IdentityMessage ident ReadiedIdentityByGame

instance Readyable AllyId where
  readied ally = AllyMessage ally ReadiedAlly
  readiedByGame ally = AllyMessage ally ReadiedAllyByGame

instance Readyable SupportId where
  readied support = SupportMessage support ReadiedSupport

instance Readyable UpgradeId where
  readied upgrade = UpgradeMessage upgrade ReadiedUpgrade

-- | An additional ready cost is optional: declining it leaves the card exhausted.
payAdditionalReadyCost :: ReverseQueue m => IdentityId -> Target -> m Bool
payAdditionalReadyCost you target = do
  costs <- mapMaybe (\case AdditionalReadyCost cost -> Just cost; _ -> Nothing) <$> getModifiers target
  if null costs then pure True else do
    let cost = mconcat costs
    canPay <- asYou you $ Cost.canAffordCost you (toSource target) cost
    chooseOne you $ [labeled "$label.payToReady" (payForTreachery you cost >> cardSpecific target "readyWithCostPaid" ()) | canPay]
      <> [labeled "$label.leaveExhausted" (pure ())]
    pure False

ready :: (ReverseQueue m, Readyable a) => a -> m ()
ready = push . readied

-- | Ready it as the game or an encounter card does: "cannot be readied by player card
-- effects" (Unnatural Storm) does not stop it.
readyByGame :: (ReverseQueue m, Readyable a) => a -> m ()
readyByGame = push . readiedByGame

-- | What can be exhausted.
class Exhaustible a where
  exhausted :: a -> Message

instance Exhaustible IdentityId where
  exhausted ident = IdentityMessage ident Msg.ExhaustedIdentity

instance Exhaustible AllyId where
  exhausted ally = AllyMessage ally Msg.ExhaustedAlly

instance Exhaustible UpgradeId where
  exhausted upgrade = UpgradeMessage upgrade ExhaustedUpgrade

instance Exhaustible SupportId where
  exhausted support = SupportMessage support ExhaustedSupport

exhaust :: (ReverseQueue m, Exhaustible a) => a -> m ()
exhaust = push . exhausted

-- | The ally's attack on the enemy, past the window before it.
allyAttackLands :: (ReverseQueue m, IsEnemy enemy) => AllyId -> enemy -> Damage -> m ()
allyAttackLands ally enemy = push . AllyMessage ally . AllyAttackLands (toEnemy enemy)

-- | Preserve the actual discard or victory departure through defeat interrupts.
allyLeavesByDefeat :: ReverseQueue m => AllyId -> Bool -> m ()
allyLeavesByDefeat aid victory = push $ PrepareAllyDepartureByDefeat aid $
  if victory then AddToVictoryDisplay (AllyRef aid) else RemoveFromPlay (AllyRef aid)

defeatAlly :: (ReverseQueue m) => AllyId -> m ()
defeatAlly ally = push $ AllyMessage ally AllyDefeated

-- | The hero's basic powers, ATK or THW given the bonus, and the defense applied.
basicAttack, basicThwart :: (ReverseQueue m) => IdentityId -> Natural -> m ()
basicAttack ident = push . IdentityMessage ident . SideMessage . BasicAttack
basicThwart ident = push . IdentityMessage ident . SideMessage . BasicThwart

-- | The basic attack's damage dealt to the target chosen, past the interrupts to it.
basicAttackOn :: (ReverseQueue m) => IdentityId -> EnemyId -> Natural -> m ()
basicAttackOn ident enemy = push . IdentityMessage ident . SideMessage . BasicAttackOn enemy

defenseApplied :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> m ()
defenseApplied ident = push . IdentityMessage ident . SideMessage . DefenseApplied 0 . toEnemy

-- | The identity is defeated, past the window for it ("would be defeated").
defeatIdentity :: (ReverseQueue m) => IdentityId -> m ()
defeatIdentity ident =
  pushAll [CheckWindows [Window Would $ W.IdentityDefeat ident], IdentityMessage ident IdentityDefeated]

-- | Preserve the cause of lethal damage for interrupts restricted to enemy attacks.
defeatIdentityFromDamage :: (ReverseQueue m) => IdentityId -> Source -> Damage -> m ()
defeatIdentityFromDamage ident source damage = do
  pushAll [CheckWindows [Window Would $ W.IdentityDefeat ident, Window Would $ W.IdentityDefeatFromDamage ident source damage], IdentityMessage ident IdentityDefeated]
  when (damageSource damage `elem` [FromAttack, FromOverkill]) $
    for_ (sourceEnemy source) $ \enemy -> cardSpecific GameRef "enemyDefeatedIdentity" (ident, enemy)

-- | The player's turn is over: what lasts for the turn ends.
identityEndedTurn :: (ReverseQueue m) => IdentityId -> m ()
identityEndedTurn = push . IdentityEndedTurn

-- | The identity flips: a hero to alter-ego, an alter-ego to a hero form of the player's
-- choice.
flipIdentity :: (ReverseQueue m) => IdentityId -> m ()
flipIdentity ident = do
  hero <- Matchers.identityMatches HeroIdentity ident
  if hero then changeToAlterEgo ident else changeToHeroForm ident

changeToAlterEgo :: (ReverseQueue m) => IdentityId -> m ()
changeToAlterEgo ident = changeToForm ident B

-- | An instruction to change to hero form has no effect on an identity already
-- in hero form. Choosing another hero form is a separate permission.
changeToHero :: ReverseQueue m => IdentityId -> m ()
changeToHero ident = Matchers.identityMatches HeroIdentity ident >>= \hero -> unless hero (changeToHeroForm ident)

-- | To a hero form other than the one showing: from alter-ego the player picks one, in
-- one hero form Ant-Man changes to the other ("change to your other hero form").
changeToHeroForm :: (ReverseQueue m) => IdentityId -> m ()
changeToHeroForm ident = push $ IdentityMessage ident ChooseHeroForm

changeToForm :: (ReverseQueue m) => IdentityId -> Side -> m ()
changeToForm ident = push . IdentityMessage ident . ChangedToForm

-- * Additional forms
--
-- Spectrum's energy forms are upgrades in play, the one she is in faceup (MTS rulebook
-- p.2). Changing one is changing form for what answers it, not the once-a-turn flip.

-- | The energy form the player is in: their faceup energy form upgrade.
faceupEnergyForm :: (ReverseQueue m) => IdentityId -> m (Maybe UpgradeId)
faceupEnergyForm ident =
  headMay <$> selectList (UpgradeWithKeyword EnergyForm <> UpgradeControlledBy (IdentityWithId ident) <> NotUpgrade FacedownUpgrade)

-- | Whether the player is in the energy form of this card.
isInEnergyForm :: (ReverseQueue m) => IdentityId -> CardDef -> m Bool
isInEnergyForm ident def =
  notNull <$> selectList (UpgradeIs def <> UpgradeControlledBy (IdentityWithId ident) <> NotUpgrade FacedownUpgrade)

-- | Monica Rambeau's Setup: the player's set-aside energy forms into play, facedown.
putEnergyFormsIntoPlay :: (ReverseQueue m) => IdentityId -> m ()
putEnergyFormsIntoPlay ident = do
  cards <- filter (\c -> EnergyForm `elem` cdKeywords (getCardDef c) && pcOwner c == Just ident) <$> getSetAsidePlayerCards
  for_ cards \c -> do
    removeFromGame (CardIdRef $ pcCardId c)
    putIntoPlay ident c
    upgradeStep (UpgradeId $ unCardId $ pcCardId c) FlipUpgradeFaceDown

-- | "Change energy forms": one facedown turned faceup, the faceup one facedown (R1).
changeEnergyForm :: (ReverseQueue m) => IdentityId -> m ()
changeEnergyForm ident = do
  stuck <- elem CannotChangeEnergyForm <$> getModifiers (IdentityRef ident)
  facedown <- selectList (UpgradeWithKeyword EnergyForm <> UpgradeControlledBy (IdentityWithId ident) <> FacedownUpgrade)
  unless stuck $ chooseOne ident [targeting (UpgradeRef u) (flipToEnergyForm ident u) | u <- facedown]

-- | "Change to <this> energy form": nothing changes if she is in it already (R2).
changeToEnergyForm :: (ReverseQueue m) => IdentityId -> CardDef -> m ()
changeToEnergyForm ident def = do
  stuck <- elem CannotChangeEnergyForm <$> getModifiers (IdentityRef ident)
  forms <- selectList (UpgradeIs def <> UpgradeControlledBy (IdentityWithId ident) <> FacedownUpgrade)
  unless stuck $ for_ (headMay forms) (flipToEnergyForm ident)

-- | The energy form turned faceup, the one she was in facedown, and the window after
-- she changes form.
flipToEnergyForm :: (ReverseQueue m) => IdentityId -> UpgradeId -> m ()
flipToEnergyForm ident u = do
  current <- faceupEnergyForm ident
  for_ current (`upgradeStep` FlipUpgradeFaceDown)
  upgradeStep u FlipUpgradeFaceUp
  checkWindows [Window After $ W.EnergyFormChanged ident u]

-- | Ask the mass-form card to flip itself in place.
flipMassForm :: (ReverseQueue m) => IdentityId -> UpgradeId -> m ()
flipMassForm ident uid = do
  canChange <- not . null <$> selectList (IdentityWithId ident <> IdentityCanChangeForm)
  locked <- elem CannotChangeMassForm <$> getModifiers ident
  when (canChange && not locked) $ cardSpecific uid "flipMassForm" ()

massFormChanged :: (ReverseQueue m) => IdentityId -> UpgradeId -> m ()
massFormChanged ident uid = checkWindows [Window After $ W.MassFormChanged ident uid]

isInMassForm :: (HasGame m) => IdentityId -> CardDef -> m Bool
isInMassForm ident def = not . null <$> selectList (UpgradeIs def <> UpgradeControlledBy (IdentityWithId ident))

putMassFormIntoPlay :: (ReverseQueue m) => IdentityId -> m ()
putMassFormIntoPlay ident = do
  cards <- filter (\c -> MassForm `elem` cdKeywords (getCardDef c) && pcOwner c == Just ident) <$> getSetAsidePlayerCards
  for_ cards \c -> do
    removeFromGame (CardIdRef $ pcCardId c)
    putIntoPlay ident c

-- | Power Down: every energy form facedown, which is not a change of form (R3).
turnEnergyFormsFacedown :: (ReverseQueue m) => IdentityId -> m ()
turnEnergyFormsFacedown ident = faceupEnergyForm ident >>= traverse_ (`upgradeStep` FlipUpgradeFaceDown)

-- | Declare a basic defender without exhausting it, then apply the additional DEF
-- for this attack. The enemy must already have begun its attack (its When window).
declareHeroDefenseWithoutExhausting :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> Natural -> m ()
declareHeroDefenseWithoutExhausting ident enemy bonus = identityStep ident (SideMessage $ DefendedWithoutExhausting bonus $ toEnemy enemy)

-- | Use ATK for this attack's basic defense calculation, even when a defense
-- event has not yet declared that basic defense.
useAttackForDefense :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> m ()
useAttackForDefense ident enemy = cardSpecific GameRef "useAttackForDefense" (ident, toEnemy enemy)

-- * Attacks and defense

enemyAttacksAlly :: (ReverseQueue m, IsEnemy enemy) => enemy -> IdentityId -> AllyId -> m ()
enemyAttacksAlly enemy ident ally = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAttacksAlly ident ally)
  EnemyMinionId mid -> MinionMessage mid (MinionAttacksAlly ident ally)
  EnemyAttachmentId _ -> CheckWindows []

-- | An explicit character attack, with boost effects resolved by the given player.
-- Activations triggered during another activation wait for it to finish.
villainAttacksCharacter :: (ReverseQueue m, IsCharacter target) => VillainId -> IdentityId -> target -> m ()
villainAttacksCharacter vid you target = villainAttacksCharacterThen vid you target (pure ())

-- | Finish an explicit character attack and its windows before its continuation.
villainAttacksCharacterThen :: (ReverseQueue m, IsCharacter target) => VillainId -> IdentityId -> target -> m () -> m ()
villainAttacksCharacterThen vid you target afterward = do
  previous <- getActivePlayerId
  enemyActivationAfterCurrent vid do
    setActivePlayer you
    villainStep vid (VillainAttacksCharacter you $ toCharacter target)
    setActivePlayer previous
    afterward

-- | An explicit character attack without dealing a new boost card for it.
villainAttacksCharacterWithoutBoost :: (ReverseQueue m, IsCharacter target) => VillainId -> IdentityId -> target -> m ()
villainAttacksCharacterWithoutBoost vid you target = do
  previous <- getActivePlayerId
  enemyActivationAfterCurrent vid do
    setActivePlayer you
    cardSpecific vid "attackCharacterWithoutBoost" (you,toCharacter target)
    setActivePlayer previous

-- | The explicit character attack also gains the listed keywords as it begins.
villainAttacksCharacterGaining :: (ReverseQueue m, IsCharacter target) => VillainId -> IdentityId -> target -> [Keyword] -> m ()
villainAttacksCharacterGaining vid you target keywords = do
  previous <- getActivePlayerId
  enemyActivationAfterCurrent vid do
    setActivePlayer you
    villainStep vid (VillainAttacksCharacterGaining you (toCharacter target) keywords)
    setActivePlayer previous

-- | An explicit attack's ATK bonus and keywords belong only to that activation.
villainAttacksCharacterWithBonus :: (ReverseQueue m, IsCharacter target) => VillainId -> IdentityId -> target -> Int -> [Keyword] -> m ()
villainAttacksCharacterWithBonus vid you target bonus keywords = do
  previous <- getActivePlayerId
  enemyActivationAfterCurrent vid do
    setActivePlayer you
    villainStep vid (VillainAttacksCharacterWithBonus you (toCharacter target) bonus keywords)
    setActivePlayer previous

-- | The enemy's attack is defended by the character.
enemyDefendedBy :: (ReverseQueue m, IsEnemy enemy) => enemy -> CharacterId -> m ()
enemyDefendedBy enemy character = do
  modifiers <- getModifiers (enemyRef $ toEnemy enemy)
  let allowed = AttacksCannotBeDefended `notElem` modifiers && all (\case OnlyIdentityCanDefend who -> character == IdentityCharacter who; _ -> True) modifiers
  characterAllowed <- case character of
    AllyCharacter aid -> canAllyDefendEnemy (toEnemy enemy) aid
    _ -> pure True
  when (allowed && characterAllowed) $ push case toEnemy enemy of
    EnemyVillainId vid -> VillainMessage vid (VillainDefendedBy character)
    EnemyMinionId mid -> MinionMessage mid (MinionDefendedBy character)
    EnemyAttachmentId _ -> CheckWindows []

-- | Resolve this particular attack, then report its final attack snapshot (or
-- Nothing if no attack occurred) to the source's "attackCompleted" step.
enemyAttacksWithCompletion :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> IdentityId -> source -> m ()
enemyAttacksWithCompletion enemy ident source = cardSpecific GameRef "enemyAttacksWithCompletion" (toEnemy enemy, ident, toSource source)

-- | A caller-owned attack token identifies its completion even when it is canceled.
enemyAttacksWithCompletionId :: (ReverseQueue m, IsEnemy enemy, IsRef source) => CardId -> enemy -> IdentityId -> source -> m ()
enemyAttacksWithCompletionId nonce enemy ident source = cardSpecific GameRef "enemyAttacksWithCompletionId" (nonce,toEnemy enemy,ident,toSource source)

eventOwnedBy :: ReverseQueue m => EventId -> IdentityId -> m ()
eventOwnedBy eid = cardSpecific (EventRef eid) "eventOwner"

enemyAttacks :: (ReverseQueue m, IsEnemy enemy) => enemy -> IdentityId -> m ()
enemyAttacks enemy ident = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAttacks ident)
  EnemyMinionId mid -> MinionMessage mid (MinionAttacks ident)
  EnemyAttachmentId _ -> CheckWindows []

-- | The villain attacks the player, the attack gaining the keywords as it is declared
-- (Avatar of Death: "that attack gains overkill and piercing").
-- ponytail: a minion's attack gains none, no card asks one to
enemyAttacksGaining :: (ReverseQueue m, IsEnemy enemy) => enemy -> IdentityId -> [Keyword] -> m ()
enemyAttacksGaining enemy ident keywords = case toEnemy enemy of
  EnemyVillainId vid -> push $ VillainMessage vid (VillainAttacksGaining ident keywords)
  e -> enemyAttacks e ident

-- | The enemy's retaliate against the character that attacked it.
enemyRetaliates :: (ReverseQueue m, IsEnemy enemy) => enemy -> CharacterId -> m ()
enemyRetaliates enemy = push . Msg.enemyRetaliates (toEnemy enemy)

-- | The character the attack is against is told it was attacked.
-- | "If a defending ally is defeated before damage from the attack is dealt (such as
-- through a Boost ability), the attack is considered undefended" (RR Defend): against
-- the character it targeted.
undefendedIfDefenderGone :: (HasGame m) => Attack -> m Attack
undefendedIfDefenderGone a = case attackCharacter a of
  AllyCharacter ally | attackDefended a -> do
    here <- notNull <$> selectList (AllyWithId ally)
    pure $ if here then a else a {attackDefended = False, attackCharacter = fromMaybe (attackCharacter a) (attackTarget a)}
  _ -> pure a

-- | Let the player review the resolved boosts before applying an enemy attack.
confirmEnemyAttack :: ReverseQueue m => Attack -> m ()
confirmEnemyAttack a = do
  you <- getYou
  reduction <- case attackCharacter a of
    IdentityCharacter ident -> project PlayerIdentityDamageReduction ident
    AllyCharacter ally -> sum . mapMaybe (\case AttackDamageReduction n -> Just n; _ -> Nothing) <$> getModifiers (AllyRef ally)
    _ -> pure 0
  let amount = subtractNatural reduction (attackDamageDealt a)
  chooseOnePromptedAbout you "$label.resolveEnemyAttack" (enemyRef $ attackEnemy a)
    [labeled (countLabel "applyAttackDamage" amount) $ wasAttacked a]

wasAttacked :: (ReverseQueue m) => Attack -> m ()
wasAttacked original = do
  modifiers <- getModifiers (characterRef $ attackCharacter original)
  let lost = [keyword | AttacksAgainstLoseKeyword keyword <- modifiers]
      a = original {attackPiercing = attackPiercing original && Piercing `notElem` lost, attackRanged = attackRanged original && Ranged `notElem` lost, attackOverkill = attackOverkill original && Overkill `notElem` lost}
  push case attackCharacter a of
    IdentityCharacter ident -> IdentityMessage ident (IdentityWasAttacked a)
    AllyCharacter ally -> AllyMessage ally (AllyWasAttacked a)
    _ -> Msg.cardSpecific GameRef "enemyCharacterWasAttacked" a

declareDefense :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> DefensePriority -> m ()
declareDefense ident enemy = push . DeclareDefense ident (toEnemy enemy)

-- | Defenders are asked for again, the players who have declined not asked.
declareDefenseWithout :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> DefensePriority -> [IdentityId] -> m ()
declareDefenseWithout ident enemy priority = push . DeclareDefenseWithout ident (toEnemy enemy) priority

-- * Villains

villainSchemes :: (ReverseQueue m) => VillainId -> m ()
villainSchemes vid = push $ VillainMessage vid VillainSchemes

-- | The villain schemes, the activation against the player given rather than the active
-- player: a card they reveal outside their activation (Avatar of Death).
villainSchemesAgainst :: (ReverseQueue m) => VillainId -> IdentityId -> m ()
villainSchemesAgainst vid = push . VillainMessage vid . VillainSchemesAgainst

villainSchemesWithoutBoostAgainst :: ReverseQueue m => VillainId -> IdentityId -> m ()
villainSchemesWithoutBoostAgainst vid you = do
  cardSpecific vid "schemeWithoutBoost" True
  villainSchemesAgainst vid you

advanceVillain :: (ReverseQueue m) => VillainId -> m ()
advanceVillain vid = villainStep vid VillainAdvanced

defeatVillain :: (ReverseQueue m) => VillainId -> m ()
defeatVillain vid = do
  -- Would-be-defeated replacements have already chosen to defeat the villain. Only
  -- now open the actual defeat interrupt, before advancing/removing its stage.
  attached <- enemyDefeatUpgrades (EnemyVillainId vid)
  pending <- withQueueM $ \q -> pure [(damage, source) | CheckWindows ws <- q, W.Window After (W.DefeatedVillain _ damage source _) <- ws]
  let (damage, source) = fromMaybe (toDamage 0 FromAbility, GameRef) (headMay pending)
  checkWindows [W.Window When $ W.DefeatedEnemyWithUpgrades (EnemyVillainId vid) attached damage source]
  villainStep vid VillainDefeated

-- | The ability of the source has resolved all it does ('AbilityResolved').
abilityResolved :: (ReverseQueue m) => Source -> m ()
abilityResolved = push . AbilityResolved

-- | "Swap <him> with a random set-aside <villain>" ('SwapVillain').
swapVillain :: (ReverseQueue m) => VillainId -> m ()
swapVillain = push . SwapVillain

-- | The villain card is replaced by the one given ('ReplaceVillain').
replaceVillain :: (ReverseQueue m) => VillainId -> CardCode -> ReplaceKind -> m ()
replaceVillain vid code = push . ReplaceVillain vid code

-- | "<The villain> activates against you": he attacks a hero, and schemes against an
-- alter-ego.
villainActivatesAgainst :: (ReverseQueue m) => VillainId -> IdentityId -> m ()
villainActivatesAgainst vid you = do
  checkWindows [Window Would $ W.VillainActivationWouldBegin vid you]
  cardSpecific GameRef "resolveVillainActivation" (vid, you)

-- | Give the coming activation one extra boost card; status cancellation consumes it.
additionalActivationBoost :: ReverseQueue m => VillainId -> IdentityId -> m ()
additionalActivationBoost vid _you = cardSpecific vid "additionalActivationBoost" (1 :: Natural)

-- | A minion activates against the player in their form when activation begins.
minionActivatesAgainst :: ReverseQueue m => MinionId -> IdentityId -> m ()
minionActivatesAgainst mid you = cardSpecific GameRef "minionActivatesAgainst" (mid, you)

-- | Resolve a prescribed declaration after its "would activate" window. Internal
-- aliases use this to preserve the original type without offering that window twice.
resolveVillainActivationAfterWould :: (ReverseQueue m) => VillainId -> VillainMessage -> m ()
resolveVillainActivationAfterWould vid = cardSpecific (VillainRef vid) "activationAfterWould"

-- | "The villain" of a card a player plays or an ability they trigger: the active
-- villain, or the villain of their area they choose while a card says so (Focused
-- Defense, MTS rulebook p.11).
withTheVillain :: (ReverseQueue m) => IdentityId -> (VillainId -> m ()) -> m ()
withTheVillain you = withTheVillainIfAny you . traverse_

-- | 'withTheVillain', the body also run, with none, when no villain is in play (as
-- between one Kang and the next).
withTheVillainIfAny :: (ReverseQueue m) => IdentityId -> (Maybe VillainId -> m ()) -> m ()
withTheVillainIfAny you body = do
  choose <- elem PlayersChooseTheVillain <$> getModifiers GameRef
  villains <- selectList AnyVillain
  case villains of
    _ : _ : _ | choose -> chooseOnePrompted you "$label.chooseTheVillain" [targeting (VillainRef v) (body $ Just v) | v <- villains]
    _ -> body =<< selectOne ActiveVillain

changeVillainHitPoints :: (ReverseQueue m) => VillainId -> Int -> m ()
changeVillainHitPoints vid = villainStep vid . VillainHitPointsChanged

-- | A boost card dealt facedown to the enemy, villain or minion: "give this enemy another
-- boost card" as its boost cards are turned up.
dealBoost :: (ReverseQueue m, IsRef enemy) => enemy -> m ()
dealBoost = push . DealBoost . toTarget

revealBoostCard :: (ReverseQueue m, IsEnemy enemy) => EncounterCard -> enemy -> m ()
revealBoostCard card = push . RevealBoostCard card . toEnemy

-- | Turn the boost card faceup without resolving its boost ability.
revealBoostCardWithoutAbility :: (ReverseQueue m, IsEnemy enemy) => EncounterCard -> enemy -> m ()
revealBoostCardWithoutAbility card = push . RevealBoostCardWithoutAbility card . toEnemy

clearBoosts :: (ReverseQueue m) => m ()
clearBoosts = push ClearBoosts

-- | The boost card stays in play, under the player.
putBoostIntoPlay :: (ReverseQueue m, IsRef card) => card -> IdentityId -> m ()
putBoostIntoPlay card = push . PutBoostIntoPlay (toTarget card)

-- | "Boost: reveal this card": the boost card is revealed for the player.
revealBoost :: (ReverseQueue m, IsRef card) => card -> IdentityId -> m ()
revealBoost card = push . RevealBoost (toTarget card)

-- | "+n ATK (or SCH) for this activation": for the villain's next activation, which follows.
activationBonus :: (ReverseQueue m) => VillainId -> Natural -> m ()
activationBonus vid = push . VillainMessage vid . VillainActivationBonus

-- | More boost icons for the villain's activation.
addBoost :: (ReverseQueue m) => VillainId -> Natural -> m ()
addBoost vid = push . VillainMessage vid . VillainAddBoost

-- | More boost icons for the activation of the enemy, villain or minion: "this card gets
-- +n boost icons for this activation".
addEnemyBoost :: (ReverseQueue m, IsEnemy enemy) => enemy -> Natural -> m ()
addEnemyBoost enemy n = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAddBoost n)
  EnemyMinionId mid -> MinionMessage mid (MinionAddBoost n)
  EnemyAttachmentId _ -> CheckWindows []

-- | "The attack gains overkill" (piercing, ranged): the enemy's attack under way.
attackGains :: (ReverseQueue m, IsEnemy enemy) => enemy -> Keyword -> m ()
attackGains enemy k = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAttackGains k)
  EnemyMinionId mid -> MinionMessage mid (MinionAttackGains k)
  EnemyAttachmentId _ -> CheckWindows []

-- | "That minion attacks another enemy" (Moondragon).
minionAttacksEnemy :: (ReverseQueue m, IsEnemy enemy) => MinionId -> enemy -> m ()
minionAttacksEnemy mid enemy = do
  you <- getActivePlayerId
  minionAttacksEnemyAs mid you enemy

-- | An explicit enemy target, with the given player resolving boost abilities.
minionAttacksEnemyAs :: (ReverseQueue m, IsEnemy enemy) => MinionId -> IdentityId -> enemy -> m ()
minionAttacksEnemyAs mid you enemy = do
  previous <- getActivePlayerId
  enemyActivationAfterCurrent mid do
    setActivePlayer you
    minionStep mid (MinionAttacksEnemy $ toEnemy enemy)
    setActivePlayer previous

-- | "That enemy gets +X ATK for that attack" (or -X): the enemy's attack under way.
changeAttack :: (ReverseQueue m, IsEnemy enemy) => enemy -> Int -> m ()
changeAttack enemy n = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAttackChanged n)
  EnemyMinionId mid -> MinionMessage mid (MinionAttackChanged n)
  EnemyAttachmentId _ -> CheckWindows []

-- | "Prevent all damage from this attack": the enemy's attack under way.
preventAttackDamage :: (ReverseQueue m, IsEnemy enemy) => enemy -> m ()
preventAttackDamage enemy = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid VillainAttackDamagePrevented
  EnemyMinionId mid -> MinionMessage mid MinionAttackDamagePrevented
  EnemyAttachmentId _ -> CheckWindows []

-- | "Cancel that attack", as a defense, from the window as the enemy initiates it
-- (Grasping Tendrils): no boost card is dealt and no damage done, and the hero is the one
-- who defended it (RR FAQ), so what answers "after you defend" may.
cancelAttackAsDefense :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> m ()
cancelAttackAsDefense ident (toEnemy -> enemy) = flip insteadOf defended \case
  VillainMessage vid (VillainBeginAttack _) -> enemy == EnemyVillainId vid
  VillainMessage vid (VillainBeginAttackWithoutBoost _) -> enemy == EnemyVillainId vid
  MinionMessage mid (MinionBeginAttack _) -> enemy == EnemyMinionId mid
  _ -> False
 where
  defended = do
    enemyDefendedBy enemy (IdentityCharacter ident)
    checkWindows [Window After (W.HeroDefends ident enemy), Window After (W.DefendedWithoutDamage ident enemy)]

-- | Cancel the activation in its opening window. This is not a defense. Keep the
-- cleanup, and never cancel a later activation. A canceled activation does not
-- trigger responses to its completion.
cancelEnemyActivation :: (HasQueue m, IsEnemy enemy) => enemy -> m ()
cancelEnemyActivation (toEnemy -> enemy) = withQueue_ $ \messages ->
  let (current, rest) = break activationEnded messages
      remaining = case rest of
        CheckWindows windows : later ->
          [CheckWindows kept | let kept = filter (not . endedWindow) windows, notNull kept] <> later
        _ -> rest
   in mapMaybe keep current <> remaining
 where
  endedWindow = \case
    Window After (W.EnemyActivation e _) -> e == enemy
    _ -> False
  activationEnded = \case
    CheckWindows windows -> any endedWindow windows
    _ -> False
  keep = \case
    VillainMessage vid step | enemy == EnemyVillainId vid -> case step of
      VillainBeginAttack _ -> Nothing
      VillainBeginAttackWithoutBoost _ -> Nothing
      VillainBeginScheme -> Nothing
      VillainSchemed -> Nothing
      VillainFlipBoostCards -> Nothing
      VillainCheckAdditionalBoosts -> Nothing
      VillainAttacked -> Nothing
      _ -> Just (VillainMessage vid step)
    MinionMessage mid step | enemy == EnemyMinionId mid -> case step of
      MinionBeginAttack _ -> Nothing
      MinionSchemed -> Nothing
      MinionFlipBoostCards -> Nothing
      MinionAttacked -> Nothing
      _ -> Just (MinionMessage mid step)
    DealBoost target | target == enemyRef enemy -> Nothing
    DeclareDefense _ e _ | e == enemy -> Nothing
    CheckWindows windows -> case filter (\case Window _ (W.EnemyAttack e _) -> e /= enemy; Window _ (W.EnemyAttackAlly e _ _) -> e /= enemy; Window _ (W.EnemyAttackOnEnemy e _) -> e /= enemy; _ -> True) windows of
      [] -> Nothing
      remaining -> Just (CheckWindows remaining)
    message -> Just message

-- | The villain turns to its other face, then its "When Revealed" is answered.
flipVillain :: (ReverseQueue m) => VillainId -> m ()
flipVillain vid = do
  villainStep vid FlipVillain
  checkWindows [Window When $ W.RevealVillain vid W.RevealedFromVillain]

-- * Minions

-- | The minion's activation in the alter-ego's villain phase: a confused one only
-- loses the confusion, otherwise it schemes ('minionSchemed').
minionSchemes :: (ReverseQueue m) => MinionId -> m ()
minionSchemes mid = push $ MinionMessage mid MinionSchemes

-- | A custom minion departure answers replacement interrupts before its own
-- cleanup step. The step is addressed to the same minion.
minionLeavingPlayStep :: ReverseQueue m => MinionId -> Text -> m ()
minionLeavingPlayStep mid key = do
  checkWindows [Window Would $ W.MinionLeavingPlay mid]
  push $ ResolveMinionLeavingPlay mid (CardSpecific (MinionRef mid) key $ toJSON ())

-- | The minion already answered its leave-play replacement window.
removeMinionFromGameWithoutLeaveWindow :: ReverseQueue m => MinionId -> m ()
removeMinionFromGameWithoutLeaveWindow mid = push $ ResolveMinionLeavingPlay mid (RemoveFromGame $ MinionRef mid)

-- | Continue a defeated minion's departure after its replacement interrupts.
resolveMinionLeavingPlay :: ReverseQueue m => MinionId -> m ()
resolveMinionLeavingPlay mid = push $ ResolveMinionLeavingPlay mid (MinionMessage mid MinionLeavesPlay)

healAllDamage :: (ReverseQueue m) => MinionId -> m ()
healAllDamage mid = push $ MinionMessage mid MinionHealAllDamage

-- | A reveal effect can choose a different engagement before the normal entry
-- engagement resolves. The chosen player replaces that pending engagement.
redirectMinionEngagement :: ReverseQueue m => MinionId -> IdentityId -> m ()
redirectMinionEngagement mid you = do
  cancelMatchingMessage $ \case
    MinionMessage other (MinionEngagedIdentity _) -> other == mid
    _ -> False
  engageMinion mid you

-- | The minion engages the player, leaving the player it was engaged with.
engageMinion :: (ReverseQueue m) => MinionId -> IdentityId -> m ()
engageMinion minion = pushAll . Msg.engageMinion minion

-- | Replace an engagement whose before-engagement interrupt is resolving.
redirectPendingMinionEngagement :: ReverseQueue m => MinionId -> IdentityId -> m ()
redirectPendingMinionEngagement mid you = do
  cardSpecific GameRef "aoaRedirectEngagementWindow" (mid, you)
  withQueue_ $ map redirect
 where
  redirect = \case
    CheckWindows windows -> CheckWindows $ map (\case
      Window When (W.EngagedMinion _ other) | other == mid -> Window When (W.EngagedMinion you mid)
      window -> window) windows
    IdentityMessage _ (MinionEngaged other) | other == mid -> IdentityMessage you (MinionEngaged mid)
    MinionMessage other (MinionEngagedIdentity _) | other == mid -> MinionMessage mid (MinionEngagedIdentity you)
    msg -> msg

-- | Continue engagement only if the minion survived a preceding check.
engageMinionIfInPlay :: (ReverseQueue m) => MinionId -> IdentityId -> m ()
engageMinionIfInPlay minion you = cardSpecific GameRef "engageMinionIfInPlay" (minion, you)

defeatMinion :: (ReverseQueue m) => MinionId -> m ()
defeatMinion = defeatMinionBy GameRef

-- | Defeat without damage, retaining the source for defeat interrupts and responses.
defeatMinionBy :: (ReverseQueue m, IsRef source) => source -> MinionId -> m ()
defeatMinionBy source mid = do
  blocked <- elem CannotBeDefeated <$> getModifiers (MinionRef mid)
  unless blocked $ push $ MinionMessage mid $ MinionDefeatedWithExcess (toSource source) (toDamage 0 FromAbility) 0

-- | The player is no longer engaged with the minion.
minionDisengages :: (ReverseQueue m) => IdentityId -> MinionId -> m ()
minionDisengages ident = push . IdentityMessage ident . MinionDisengaged

-- | The minion's attack against the player, past its would window.
minionBeginsAttack :: (ReverseQueue m) => MinionId -> IdentityId -> m ()
minionBeginsAttack mid = push . MinionMessage mid . MinionBeginAttack

minionAttacked :: (ReverseQueue m) => MinionId -> m ()
minionAttacked mid = push $ MinionMessage mid MinionAttacked

-- | Replace the current attack's remaining steps with attacks whose steps resolve
-- together, as Whirlwind's rules require. The original attack's cleanup remains.
simultaneousMinionAttacks :: ReverseQueue m => MinionId -> IdentityId -> [IdentityId] -> m ()
simultaneousMinionAttacks mid you others = do
  insteadOf (\case DeclareDefense who (EnemyMinionId enemy) _ -> who == you && enemy == mid; _ -> False) $
    cardSpecific GameRef "beginSimultaneousMinionAttacks" (mid, you, others)
  withQueue_ $ \messages ->
    let (current, later) = break (== MinionMessage mid MinionEndAttack) messages
        keep = \case
          MinionMessage enemy MinionFlipBoostCards -> enemy /= mid
          MinionMessage enemy MinionAttacked -> enemy /= mid
          CheckWindows [Window After (W.EnemyAttack (EnemyMinionId enemy) who)] -> enemy /= mid || who /= you
          _ -> True
     in filter keep current <> later

minionEndsAttack :: (ReverseQueue m) => MinionId -> m ()
minionEndsAttack mid = push $ MinionMessage mid MinionEndAttack

-- | The minion's scheme, it not confused: threat on the main scheme.
minionSchemed :: (ReverseQueue m) => MinionId -> m ()
minionSchemed mid = push $ MinionMessage mid MinionSchemed

-- | The ally is flipped to its other face (RR Flip).
flipAlly :: (ReverseQueue m) => AllyId -> m ()
flipAlly aid = push $ AllyMessage aid FlipAlly

-- | The ally card is put into play as a minion engaged with the player (Fallen
-- Warrior), its id the card's; a matching unique card in play keeps it out.
putAllyIntoPlayAsMinion :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
putAllyIntoPlayAsMinion ident = push . PutAllyIntoPlayAsMinion ident

-- | The ally in play is treated as a minion engaged with its controller (Beguiled),
-- its id the ally's.
allyBecomesMinion :: (ReverseQueue m) => AllyId -> m ()
allyBecomesMinion = push . AllyBecomesMinion

-- | The minion an ally was treated as is its owner's ally again.
minionBecomesAlly :: (ReverseQueue m) => MinionId -> m ()
minionBecomesAlly = push . MinionBecomesAlly

-- | Treat a minion as a controlled ally without leaving play.
takeControlOfMinion :: (ReverseQueue m) => IdentityId -> MinionId -> m ()
takeControlOfMinion you = push . TakeControlOfMinion you

-- | Restore the minion when its continuous control effect ends.
restoreControlledMinion :: (ReverseQueue m) => AllyId -> m ()
restoreControlledMinion = push . RestoreControlledMinion

-- | Whether the minion is about to leave play: its defeat, discard or removal queued
-- (its attachments leave first as it is defeated).
minionLeavingPlay :: (HasQueue m) => MinionId -> m Bool
minionLeavingPlay mid = isQueued \case
  DefeatedLeavesPlay t -> t == MinionRef mid
  RemoveFromPlay t -> t == MinionRef mid
  RemoveFromGame t -> t == MinionRef mid
  AddToVictoryDisplay t -> t == MinionRef mid
  _ -> False

-- * Attachments and environments

-- | Whatever has the attachment is told it has left play.
attachmentRemoved :: (ReverseQueue m) => AttachmentId -> m ()
attachmentRemoved = push . AttachmentRemoved

-- | The attachment is attached to the enemy ('AttachedToEnemy').
attachTo :: (ReverseQueue m, IsEnemy enemy) => AttachmentId -> enemy -> m ()
attachTo attachment enemy = do
  checkWindows [W.Window W.Would $ W.AttachmentAttachingToEnemy attachment (toEnemy enemy)]
  attachmentStep attachment (AttachedToEnemy $ toEnemy enemy)

-- | The enemy is told the attachment is attached to it.
attachedTo :: (ReverseQueue m, IsEnemy enemy) => enemy -> AttachmentId -> m ()
attachedTo enemy attachment = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (AttachedToVillain attachment)
  EnemyMinionId mid -> MinionMessage mid (AttachedToMinion attachment)
  EnemyAttachmentId _ -> CheckWindows []

-- | The attachment card is put into play attached to the enemy, without being revealed:
-- "attach that card to Zola's Mutate".
attachEncounterCard :: (ReverseQueue m, IsEnemy enemy) => EncounterCard -> enemy -> m ()
attachEncounterCard card = push . AttachEncounterCard card . toEnemy

-- | The attachment is attached to the player's identity card.
attachToIdentity :: (ReverseQueue m) => AttachmentId -> IdentityId -> m ()
attachToIdentity attachment = push . AttachmentMessage attachment . AttachedToIdentity

-- | The attachment is attached to the player's ally.
attachToAlly :: (ReverseQueue m) => AttachmentId -> AllyId -> m ()
attachToAlly attachment = push . AttachmentMessage attachment . AttachedToAlly

attachToSideScheme :: (ReverseQueue m) => AttachmentId -> SideSchemeId -> m ()
attachToSideScheme attachment = push . AttachmentMessage attachment . AttachedToSideScheme

attachToMainScheme :: (ReverseQueue m) => AttachmentId -> MainSchemeId -> m ()
attachToMainScheme attachment = push . AttachmentMessage attachment . AttachedToMainScheme

-- | Damage placed on the environment (Avengers Tower).
damageEnvironment :: (ReverseQueue m) => EnvironmentId -> Natural -> m ()
damageEnvironment eid = push . EnvironmentMessage eid . EnvironmentDamaged

attachToEnvironment :: (ReverseQueue m) => EnvironmentId -> AttachmentId -> m ()
attachToEnvironment environment = push . EnvironmentMessage environment . AttachedToEnvironment

damageAttachment :: (ReverseQueue m) => AttachmentId -> Natural -> m ()
damageAttachment attachment = push . AttachmentMessage attachment . AttachmentDamaged

-- | The environment turns to its other face.
flipEnvironment :: (ReverseQueue m) => EnvironmentId -> m ()
flipEnvironment eid = push $ EnvironmentMessage eid FlipEnvironment

-- | The attachment is attached to the character, moving there if it is attached
-- elsewhere.
attachToCharacter :: (ReverseQueue m, IsCharacter c) => AttachmentId -> c -> m ()
attachToCharacter aid c = case toCharacter c of
  IdentityCharacter i -> attachToIdentity aid i
  VillainCharacter v -> attachTo aid v
  MinionCharacter m -> attachTo aid m
  AttachmentCharacter _ -> pure ()
  AllyCharacter a -> attachToAlly aid a

-- * The Milano and the Power Stone (The Galaxy's Most Wanted)

-- | "Exhaust the Milano": any player may, as a cost or an option, whether or not they
-- control it (TGMW rulebook FAQ, p.21). Nothing happens with it exhausted or gone.
exhaustMilano :: (ReverseQueue m) => m ()
exhaustMilano = getMilano >>= traverse_ exhaust

-- | "Attach the Power Stone to …": it moves to the hero or villain, wherever it was.
attachPowerStoneTo :: (ReverseQueue m, IsCharacter c) => c -> m ()
attachPowerStoneTo c = getPowerStone >>= traverse_ (`attachToCharacter` c)

-- * Schemes

-- | The main scheme is completed: what that does is its own to say.
advanceMainScheme :: (ReverseQueue m) => MainSchemeId -> m ()
advanceMainScheme = push . AdvanceMainScheme

-- | The main scheme leaves play for the next stage of the scenario, in its own area.
nextMainScheme :: (ReverseQueue m) => MainSchemeId -> m ()
nextMainScheme = push . NextMainScheme

-- | The side scheme is defeated, past the window for it, and leaves play.
defeatSideScheme :: (ReverseQueue m) => SideSchemeId -> m ()
defeatSideScheme sid = defeatSideSchemeWith sid Nothing

-- | The side scheme is defeated by what removed its last threat.
defeatSideSchemeBy :: (ReverseQueue m, IsRef source) => SideSchemeId -> source -> m ()
defeatSideSchemeBy sid = defeatSideSchemeWith sid . Just . toSource

defeatSideSchemeWith :: (ReverseQueue m) => SideSchemeId -> Maybe Source -> m ()
defeatSideSchemeWith sid source = defeatSideSchemeWithThwart sid source False

-- | Defeat attributed to actual thwarting, rather than other threat removal.
defeatSideSchemeByThwart :: (ReverseQueue m, IsRef source) => SideSchemeId -> source -> m ()
defeatSideSchemeByThwart sid source = defeatSideSchemeWithThwart sid (Just $ toSource source) True

defeatSideSchemeWithThwart :: (ReverseQueue m) => SideSchemeId -> Maybe Source -> Bool -> m ()
defeatSideSchemeWithThwart sid source thwart = do
  you <- getYou
  pushAll
    [ CheckWindows [Window When $ W.DefeatedSideScheme sid]
    , SideSchemeMessage sid DefeatSideScheme
    , DefeatedLeavesPlay (toTarget sid)
    , CheckWindows $ [Window After $ W.DefeatedSideSchemeBy sid you source] <> [Window After $ W.DefeatedSideSchemeByThwart sid s | thwart, Just s <- [source]]
    ]

-- | What the side scheme does once what it revealed is done.
sideSchemeRevealedThen :: (ReverseQueue m) => SideSchemeId -> m ()
sideSchemeRevealedThen sid = push $ SideSchemeMessage sid SideSchemeRevealedThen

searchForAndRevealScheme :: (ReverseQueue m) => CardDef -> m ()
searchForAndRevealScheme = push . SearchForAndRevealScheme

-- * The victory display

-- | The card leaves play for the victory display, not its discard pile.
addToVictoryDisplay :: (ReverseQueue m, IsRef a) => a -> m ()
addToVictoryDisplay = push . AddToVictoryDisplay . toTarget

-- | The card is defeated and leaves play: a card with victory X for the victory display
-- (RR Victory X), any other for its discard pile.
defeatedLeavesPlay :: (ReverseQueue m, IsRef a) => a -> m ()
defeatedLeavesPlay = push . DefeatedLeavesPlay . toTarget

-- | A card out of play goes to the victory display (a defeated villain).
addCardToVictoryDisplay :: (ReverseQueue m) => EncounterCard -> m ()
addCardToVictoryDisplay = push . AddCardToVictoryDisplay

-- * The Collection (Infiltrate the Museum)

-- | The card goes faceup into The Collection, from wherever it is: a deck, a hand, a
-- discard pile, or play (leaving play for it, not discarded).
placeInCollection :: (ReverseQueue m, IsCard card) => card -> m ()
placeInCollection = push . PlaceInCollection . toCard

-- | The card in play, by its ref, goes faceup into The Collection, leaving play for it.
placeInCollectionFromPlay :: (ReverseQueue m, IsRef a) => a -> m ()
placeInCollectionFromPlay a = getsGame (`cardInPlay` toTarget a) >>= traverse_ placeInCollection

-- | The top card of the player's deck, as it is when this resolves, goes faceup into
-- The Collection.
placeTopOfDeckInCollection :: (ReverseQueue m) => IdentityId -> m ()
placeTopOfDeckInCollection ident = push $ IdentityMessage ident TopOfDeckIntoCollection

-- | A card leaves The Collection for its owner's discard pile (the encounter discard
-- pile for an encounter card).
discardFromCollection :: (ReverseQueue m, IsCard card) => card -> m ()
discardFromCollection = push . DiscardedCard . toCard

-- * Player cards

drawCards :: (ReverseQueue m) => IdentityId -> Natural -> m ()
drawCards ident = push . IdentityMessage ident . DrawCards FromDeck

discardFromHand :: (ReverseQueue m) => IdentityId -> Natural -> m ()
discardFromHand ident = discardFrom ident FromHand

-- | The player's deck is discarded from until a card matching, the target told which
-- ('WithDiscardedMatch').
discardFromDeckUntil :: (ReverseQueue m, IsRef target) => IdentityId -> CardMatcher -> target -> m ()
discardFromDeckUntil ident matcher = push . IdentityMessage ident . DiscardFromDeckUntil matcher . toTarget

addToHand :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
addToHand ident = push . IdentityMessage ident . AddToHand

-- | A player card that was nobody's, as a Captive ally set aside in an encounter set, or
-- another player's, goes into the player's hand: they own and control it from now on.
takeControlOf :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
takeControlOf ident card = addToHand ident card {pcOwner = Just ident, pcController = Just ident}

placeOnBottomOfDeck :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
placeOnBottomOfDeck ident = push . IdentityMessage ident . PlaceOnBottomOfDeck

placeOnTopOfDeck :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
placeOnTopOfDeck ident = push . IdentityMessage ident . PlaceOnTopOfDeck

shuffleDeck :: (ReverseQueue m) => IdentityId -> m ()
shuffleDeck ident = push $ IdentityMessage ident ShuffleDeck

-- | The player shuffles their discard pile into their deck (Balance the Scales).
shuffleDiscardIntoDeck :: (ReverseQueue m) => IdentityId -> m ()
shuffleDiscardIntoDeck ident = identityStep ident ShuffleIdentityDiscardBackIntoDeck

-- | The player's empty deck is made again from their discard pile, and they deal
-- themself a facedown encounter card (RR Player Deck); then the window after they reset
-- their deck.
resetDeck :: (ReverseQueue m) => IdentityId -> m ()
resetDeck ident = do
  identityStep ident ShuffleIdentityDiscardBackIntoDeck
  dealEncounterCard ident
  checkWindows [Window After $ W.PlayerDeckReset ident]

-- | The event's "Special" resolved without the card being played.
resolveEventWithoutPlaying :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
resolveEventWithoutPlaying ident = push . ResolveEventWithoutPlaying ident

focusCards :: (ReverseQueue m) => [Card] -> m ()
focusCards = push . FocusCards

unfocusCards :: (ReverseQueue m) => m ()
unfocusCards = push UnfocusCards

removeFromPlay :: (ReverseQueue m, IsRef a) => a -> m ()
removeFromPlay = push . RemoveFromPlay . toTarget

removeFromGame :: (ReverseQueue m, IsRef a) => a -> m ()
removeFromGame = push . RemoveFromGame . toTarget

drawToPrintedHandSize :: (ReverseQueue m) => IdentityId -> m ()
drawToPrintedHandSize ident = push $ IdentityMessage ident DrawToPrintedHandSize

-- | Draw up to the hand size as modified, as at the end of the player phase.
drawToHandSize :: (ReverseQueue m) => IdentityId -> m ()
drawToHandSize ident = push $ IdentityMessage ident DrawToHandLimit

-- | The cards are shown to the player only while the body runs: "look at".
lookingAt :: (ReverseQueue m) => IdentityId -> [Card] -> m () -> m ()
lookingAt ident cards body = push (ShowCardsTo ident cards) >> body >> push (HideCardsFrom ident)

-- | The cards are shown to everyone while the body runs.
focusing :: (ReverseQueue m) => [Card] -> m () -> m ()
focusing cards body = focusCards cards >> body >> unfocusCards

-- | The top cards of the player's deck are discarded, the target told which
-- ('WithDiscarded').
discardFromDeck :: (ReverseQueue m, IsRef target) => IdentityId -> Natural -> target -> m ()
discardFromDeck ident n = push . IdentityMessage ident . DiscardFrom FromDeck n . Just . toTarget

-- | Cards from the zone discarded, no one told which.
discardFrom :: (ReverseQueue m) => IdentityId -> FromZone -> Natural -> m ()
discardFrom ident zone n = push $ IdentityMessage ident $ DiscardFrom zone n Nothing

-- | The player discards between lo and hi cards from the zone, the target told which
-- ('WithDiscarded').
discardFor :: (ReverseQueue m, IsRef target) => IdentityId -> target -> FromZone -> Natural -> Natural -> m ()
discardFor ident target zone lo = push . IdentityMessage ident . DiscardFor (toTarget target) zone lo

-- | The top cards of the player's deck are discarded.
discardTopOfDeck :: (ReverseQueue m) => IdentityId -> Natural -> m ()
discardTopOfDeck ident = discardFrom ident FromDeck

-- | Costs and host cleanup discard by game rules, not by a card effect.
frameworkDiscard :: ReverseQueue m => m () -> m ()
frameworkDiscard body = do
  cardSpecific GameRef "beginFrameworkDiscard" ()
  body
  cardSpecific GameRef "endFrameworkDiscard" ()

-- | The card goes to its discard pile.
discardCard :: (ReverseQueue m, IsCard card) => card -> m ()
discardCard = push . DiscardedCard . toCard

-- | The player chooses cards in their discard pile, the target told which
-- ('WithChosen').
chooseFromDiscard ::
  (ReverseQueue m, IsRef target) => IdentityId -> target -> ChoiceRules -> Natural -> Natural -> m ()
chooseFromDiscard ident target rules lo = push . IdentityMessage ident . ChooseFromDiscard (toTarget target) rules lo

shuffleIntoDeck :: (ReverseQueue m) => IdentityId -> [PlayerCard] -> m ()
shuffleIntoDeck ident = push . IdentityMessage ident . ShuffleIntoIdentityDeck

search ::
  (ReverseQueue m) => IdentityId -> SearchSignifier -> CardMatcher -> SearchOption -> ReturnOption -> m ()
search ident signifier matcher option = push . IdentityMessage ident . Search signifier matcher option

-- | The card is put into play under the player's control, not played.
putIntoPlay :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
putIntoPlay ident card = push $ PutCardIntoPlay ident card NoPayment Nothing False

returnToHand :: (ReverseQueue m, IsRef a) => a -> m ()
returnToHand = push . ReturnToHand . toTarget

-- * Upgrades

discardUpgrade :: (ReverseQueue m) => UpgradeId -> m ()
discardUpgrade u = push $ UpgradeMessage u DiscardUpgrade

addUpgradeUses :: (ReverseQueue m) => UpgradeId -> Natural -> m ()
addUpgradeUses u = push . UpgradeMessage u . AddUpgradeUses

-- | The player is told the upgrade has come into play under their control.
upgradeCreated :: (ReverseQueue m) => IdentityId -> UpgradeId -> m ()
upgradeCreated ident = push . IdentityMessage ident . UpgradeCreated

-- | The ally or enemy is told the upgrade is attached to it.
upgradeAttachedTo :: (ReverseQueue m, IsCharacter character) => character -> UpgradeId -> m ()
upgradeAttachedTo character u = push case toCharacter character of
  AllyCharacter ally -> AllyMessage ally (AttachedUpgradeToAlly u)
  MinionCharacter mid -> MinionMessage mid (AttachedUpgradeToMinion u)
  AttachmentCharacter _ -> CheckWindows []
  VillainCharacter vid -> VillainMessage vid (AttachedUpgradeToVillain u)
  IdentityCharacter _ -> error "an upgrade on an identity is controlled, not attached"

-- | Whatever has the upgrade attached is told it has left play.
upgradeRemoved :: (ReverseQueue m) => UpgradeId -> m ()
upgradeRemoved = push . UpgradeRemoved

-- | An upgrade changes controller, without a leave-play notification.
upgradeControlLost :: (ReverseQueue m) => IdentityId -> UpgradeId -> m ()
upgradeControlLost ident = identityStep ident . UpgradeControlLost

-- | A bonus to the ally's current basic power, whose target and base amount have
-- already been chosen before its interrupt window opens.
addToAllyBasicPower :: (HasQueue m) => AllyId -> Natural -> m ()
addToAllyBasicPower aid bonus = replaceMatchingMessage raise matches
 where
  matches = \case
    AllyMessage who (AllyAttackLands _ _) -> who == aid
    AllyMessage who (AllyEvenAttackLands _ _) -> who == aid
    ResolveThreatRemoval True True _ _ (AllyRef who) _ -> who == aid
    MainSchemeMessage _ (MainSchemeBasicThwarted (AllyRef who) _) -> who == aid
    MainSchemeMessage _ (MainSchemeThwarted (AllyRef who) _) -> who == aid
    SideSchemeMessage _ (SideSchemeThwarted (AllyRef who) _) -> who == aid
    _ -> False
  raise = \case
    AllyMessage who (AllyAttackLands enemy damage) -> [AllyMessage who (AllyAttackLands enemy damage {damageAmount = damageAmount damage + bonus})]
    AllyMessage who (AllyEvenAttackLands damage shares) -> [AllyMessage who (AllyEvenAttackLands damage {damageAmount = damageAmount damage + bonus} shares)]
    ResolveThreatRemoval basic thwarting who target source amount -> [ResolveThreatRemoval basic thwarting who target source (amount + bonus)]
    MainSchemeMessage sid (MainSchemeBasicThwarted source amount) -> [MainSchemeMessage sid (MainSchemeBasicThwarted source (amount + bonus))]
    MainSchemeMessage sid (MainSchemeThwarted source amount) -> [MainSchemeMessage sid (MainSchemeThwarted source (amount + bonus))]
    SideSchemeMessage sid (SideSchemeThwarted source amount) -> [SideSchemeMessage sid (SideSchemeThwarted source (amount + bonus))]
    other -> [other]

-- | The basic power selected before an ally's current thwart interrupt window.
allyThwartUsesAttack :: (HasQueue m) => AllyId -> m Bool
allyThwartUsesAttack aid = withQueue $ \q ->
  (q, fromMaybe False $ headMay [usingAttack | CardSpecific (AllyRef who) "basicThwartUsesAttack" (fromJSON -> Success usingAttack) <- concatMap queuedMessageLeaves q, who == aid])

-- * Paying

-- | The player pays the cost for an encounter card, as a treachery asks.
payForTreachery :: (ReverseQueue m) => IdentityId -> Cost -> m ()
payForTreachery ident cost = getRandom >>= \cid -> setActiveCost $ ActiveCost cid ident ForTreachery cost NoPayment Nothing mempty Nothing

-- | The cost is begun on.
createdActiveCost :: (ReverseQueue m) => ActiveCostId -> m ()
createdActiveCost = push . CreatedActiveCost

-- | The cost is to be paid, as it says.
setActiveCost :: (ReverseQueue m) => ActiveCost -> m ()
setActiveCost = push . SetActiveCost

-- | The card is spent on the cost.
spendCardWithResources :: (ReverseQueue m) => ActiveCostId -> PlayerCard -> [Resource] -> [Resource] -> m ()
spendCardWithResources acid card generated printed = push $ SpentWithResources acid card generated printed

spendCard :: (ReverseQueue m) => ActiveCostId -> PlayerCard -> m ()
spendCard acid = push . Spent acid

-- | Part of the cost paid.
paid :: (ReverseQueue m) => Payment -> m ()
paid = push . Paid

-- | Invite another player to contribute to the current alliance payment.
contributeResources :: (ReverseQueue m) => ActiveCostId -> IdentityId -> m ()
contributeResources acid = push . ContributeResources acid

-- | Ask the contributor to declare the wild resources they just generated.
declareContributionWilds :: (ReverseQueue m) => ActiveCostId -> IdentityId -> Natural -> m ()
declareContributionWilds acid who = push . DeclareContributionWilds acid who

-- | The contributor pays this part using their own characters and cards.
contributeCost :: (ReverseQueue m) => ActiveCostId -> IdentityId -> Cost -> m ()
contributeCost acid ident = push . ContributeCost acid ident

-- | Obtain all contributions before committing non-resource costs.
prepareAllianceCosts :: (ReverseQueue m) => ActiveCostId -> [(IdentityId, Cost)] -> [Cost] -> m ()
prepareAllianceCosts acid approved = push . PrepareAllianceCosts acid approved

offerAllianceCost :: (ReverseQueue m) => ActiveCostId -> IdentityId -> Cost -> [(IdentityId, Cost)] -> [Cost] -> m ()
offerAllianceCost acid who cost approved = push . OfferAllianceCost acid who cost approved

reserveAllyHeldCard :: (ReverseQueue m) => AllyId -> PlayerCard -> m ()
reserveAllyHeldCard ally = push . ReserveAllyHeldCard ally

-- | Attach an event facedown and allow its controller to play it while the ally's
-- text is active. The event leaves its previous zone before being attached.
attachPlayableEventToAlly :: (ReverseQueue m) => AllyId -> PlayerCard -> m ()
attachPlayableEventToAlly ally card = do
  removeFromGame card
  push $ AttachPlayableEventToAlly ally card

-- | Play this card normally with a reduction confined to its own play and payment.
playCardWithReduction :: (ReverseQueue m) => IdentityId -> Natural -> PlayerCard -> m ()
playCardWithReduction who n card = push $ PlayCardWithReduction who card n

-- | An explicit search permission; leave the physical card in its deck until
-- payment succeeds, so declining payment preserves its original zone.
playSearchedCardWithReduction :: ReverseQueue m => IdentityId -> Natural -> PlayerCard -> m ()
playSearchedCardWithReduction who amount card = cardSpecific GameRef "playSearchedCardWithReduction" (who,amount,card)


playCardIgnoringResourceCost :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
playCardIgnoringResourceCost who card = push $ PlayCardIgnoringResourceCost who card

-- | The part of the cost is paid ('PayCost').
payCost :: (ReverseQueue m) => ActiveCostId -> Cost -> m ()
payCost acid = push . PayCost acid

-- | The resources still to pay for the cost are asked for.
checkPayment :: (ReverseQueue m) => ActiveCostId -> m ()
checkPayment = push . CheckPayment

-- | The resources are paid: the rest of the cost follows.
finishPayment :: (ReverseQueue m) => ActiveCostId -> m ()
finishPayment = push . FinishedPayment

-- | Select the actual resources spent out of the generated pool.
allocatePayment :: (ReverseQueue m) => ActiveCostId -> [Resource] -> m ()
allocatePayment acid = push . AllocatedPayment acid

-- | The cost cannot be paid: what was spent on it goes back.
abortPayment :: (ReverseQueue m) => ActiveCostId -> m ()
abortPayment = push . AbortPayment

-- | The whole cost is paid: what it was paid for follows.
paidCosts :: (ReverseQueue m) => ActiveCostId -> m ()
paidCosts = push . PaidCosts

disableActiveCost :: (ReverseQueue m) => ActiveCostId -> m ()
disableActiveCost = push . DisableActiveCost

-- | The card is played, paid for: "when you play" is answered, then it enters play and
-- the window after it opens.
cardPlayed :: (ReverseQueue m) => IdentityId -> PlayerCard -> Payment -> Maybe Window -> m ()
cardPlayed ident card payment mWindow = do
  when (cdCardType (getCardDef card) == UpgradeType) $ do
    area <- maybe (pure $ Just ident) cardPlayAreaOwner (paymentTarget payment)
    cardSpecific GameRef "upgradePlayedInArea" area
  pushAll
    [ CheckWindows (windows When)
    , IdentityMessage ident (PlayedCard card mWindow)
    , PutCardIntoPlay ident card payment mWindow True
    , CheckWindows (windows After)
    ]
 where
  windows timing = Window timing (W.CardPlayed ident card) :
    [ Window timing $ W.UpgradePlayedOnAlly ident (UpgradeId $ unCardId $ pcCardId card) aid
    | cdCardType (getCardDef card) == UpgradeType
    , Just (AllyRef aid) <- [paymentTarget payment]
    ]

-- | The ability on the target resolves, as if triggered in the windows given.
ranAbility :: (ReverseQueue m) => IdentityId -> Target -> Natural -> [W.WindowType] -> Payment -> m ()
ranAbility ident target n windows = push . RanAbility ident target n windows

-- | The ability is counted against its limit.
usedAbility :: (ReverseQueue m) => IdentityId -> Ability -> m ()
usedAbility ident = push . UsedAbility ident

-- | What has uses to spend.
class HasUses a where
  spentUse :: a -> Message

instance HasUses UpgradeId where
  spentUse u = UpgradeMessage u SpendUpgradeUse

instance HasUses SupportId where
  spentUse s = SupportMessage s SpendSupportUse

instance HasUses AllyId where
  spentUse a = AllyMessage a SpendAllyUse

spendUse :: (ReverseQueue m, HasUses a) => a -> m ()
spendUse = push . spentUse

-- * The encounter deck

putTopOfDeckIntoPlayAsDrone :: (ReverseQueue m) => IdentityId -> m ()
putTopOfDeckIntoPlayAsDrone = push . PutTopOfDeckIntoPlayAsDrone

putEncounterCardIntoPlay :: (ReverseQueue m) => IdentityId -> EncounterCard -> m ()
putEncounterCardIntoPlay ident = push . PutEncounterCardIntoPlay ident

-- | The encounter card resolves as revealed, without the windows a reveal opens:
-- put into play, as setup does.
revealEncounterCardWithoutWindows :: (ReverseQueue m) => IdentityId -> EncounterCard -> m ()
revealEncounterCardWithoutWindows ident = push . RevealedEncounterCardFrom W.RevealedFromEncounterDeck ident

shuffleEncounterDeck :: (ReverseQueue m) => m ()
shuffleEncounterDeck = push ShuffleEncounterDeck

discardTopOfEncounterDeck :: (ReverseQueue m) => Natural -> m ()
discardTopOfEncounterDeck n = push $ DiscardTopOfEncounterDeck n Nothing

-- | The cards go to the set-aside area.
setAside :: (ReverseQueue m) => [Card] -> m ()
setAside = push . SetAside

-- | Cards are discarded from the zone until one matches, the target told which
-- ('WithDiscardedMatch').
discardUntil :: (ReverseQueue m, IsRef target) => FromZone -> CardMatcher -> target -> m ()
discardUntil zone matcher = push . DiscardUntil zone matcher . toTarget

-- | "Discard cards from the top of the encounter deck until a … is discarded, and put it
-- into play" for the player ('DiscardUntilIntoPlay').
discardUntilIntoPlay :: (ReverseQueue m) => IdentityId -> CardMatcher -> m ()
discardUntilIntoPlay ident = push . DiscardUntilIntoPlay ident

-- | The player draws and reveals an encounter card.
drawAndRevealEncounterCard :: (ReverseQueue m) => IdentityId -> m ()
drawAndRevealEncounterCard ident = push $ DrawAndRevealEncounterCard ident

-- | The top cards of the encounter deck are looked at, the target told which
-- ('SearchFoundCards').
lookAtTopOfEncounterDeck :: (ReverseQueue m, IsRef target) => Natural -> target -> m ()
lookAtTopOfEncounterDeck n = push . LookAtTopOfEncounterDeck n . toTarget

surge :: (ReverseQueue m) => IdentityId -> m ()
surge = push . Surge

-- | The encounter card gains surge.
gainSurge :: (ReverseQueue m, IsRef a) => a -> m ()
gainSurge = push . GainSurge . toTarget

-- | Grant surge before the revealing entity exists, resolving it after that reveal.
giveRevealedCardSurge :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
giveRevealedCardSurge you card = do
  pending <- isQueued $ grantedSurgeFor $ ecCardId card
  unless (pending || Keyword.Surge `member` cdKeywords (ecCardDef card)) $
    replaceMatchingMessage (\msg -> [msg, Msg.cardSpecific GameRef "grantedSurge" (you, ecCardId card)]) revealing
 where
  revealing = \case
    RevealedEncounterCard _ other -> ecCardId other == ecCardId card
    RevealedEncounterCardFrom _ _ other -> ecCardId other == ecCardId card
    _ -> False

grantedSurgeFor :: CardId -> Message -> Bool
grantedSurgeFor cid = \case
  CardSpecific GameRef "grantedSurge" (fromJSON -> Success (_ :: IdentityId, other :: CardId)) -> cid == other
  _ -> False

-- | The player reveals the encounter card, its "when revealed" windows answered.
revealEncounterCard :: (ReverseQueue m) => IdentityId -> EncounterCard -> m ()
revealEncounterCard ident = push . RevealEncounterCard ident

-- | Reveal a found physical card after detaching it from eligible holding zones.
revealFoundEncounterCard :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
revealFoundEncounterCard you card = cardSpecific GameRef "aoaRevealFoundEncounter" (you, card)

revealEncounterCardFrom :: (ReverseQueue m) => W.RevealSource -> IdentityId -> EncounterCard -> m ()
revealEncounterCardFrom source ident = push . RevealEncounterCardFrom source ident

-- | The top cards of the encounter deck are discarded, the target told which
-- ('WithDiscarded').
discardTopOfEncounterDeckFor :: (ReverseQueue m, IsRef target) => Natural -> target -> m ()
discardTopOfEncounterDeckFor n = push . DiscardTopOfEncounterDeck n . Just . toTarget

-- | The top cards of the encounter deck are discarded, then the body does what it does
-- with them: as many as there were, a deck run out partway not refilled.
discardTopOfEncounterDeckThen :: (ReverseQueue m) => Natural -> ([EncounterCard] -> m ()) -> m ()
discardTopOfEncounterDeckThen n body = do
  cards <- take (fromIntegral n) <$> getEncounterDeck
  discardTopOfEncounterDeck n
  body cards

-- | The boost icons on the cards are counted, the target told the counts
-- ('WithBoostIcons') with the value.
countBoostIcons :: (ReverseQueue m, IsRef target, ToJSON v) => target -> v -> [EncounterCard] -> m ()
countBoostIcons target v = push . CountBoostIcons (toTarget target) (toJSON v)

-- | The boost cards turned faceup for the enemy's activation have their icons counted,
-- an amplify icon in play adding one to each (RR Amplify Icon): the enemy is told the
-- counts ('WithBoostIcons') with no value.
countActivationBoostIcons :: (ReverseQueue m, IsRef enemy) => enemy -> [EncounterCard] -> m ()
countActivationBoostIcons enemy = push . CountActivationBoostIcons (toTarget enemy)

shuffleIntoEncounterDeck :: (ReverseQueue m) => [EncounterCard] -> m ()
shuffleIntoEncounterDeck = push . ShuffleIntoEncounterDeck

-- | A card matching is found in the encounter deck or discard pile and revealed.
searchAndRevealEncounterCard :: (ReverseQueue m) => IdentityId -> CardMatcher -> m ()
searchAndRevealEncounterCard ident = push . SearchForAndRevealEncounterCard ident

-- | A card matching is found in the encounter deck, discard pile or set-aside area and
-- put into play for the player (an attachment attached as its own text says).
searchEverywhereAndPutIntoPlayEncounterCard :: (ReverseQueue m) => IdentityId -> CardMatcher -> m ()
searchEverywhereAndPutIntoPlayEncounterCard ident = push . SearchEverywhereAndPutIntoPlayEncounterCard ident

-- | A card matching is found in the encounter deck or discard pile and dealt to the
-- player facedown.
searchAndDealEncounterCard :: (ReverseQueue m) => IdentityId -> CardMatcher -> m ()
searchAndDealEncounterCard ident = push . SearchForAndDealEncounterCard ident

-- | The encounter card found is dealt to the player facedown, out of where it was.
dealFoundEncounterCard :: (ReverseQueue m) => IdentityId -> EncounterCard -> m ()
dealFoundEncounterCard ident = push . DealFoundEncounterCard ident

-- | A card matching is found in the encounter deck or discard pile and put into play.
searchAndPutIntoPlayEncounterCard :: (ReverseQueue m) => IdentityId -> CardMatcher -> m ()
searchAndPutIntoPlayEncounterCard ident = push . SearchForAndPutIntoPlayEncounterCard ident

-- | An acceleration token placed next to "the main scheme": the one of "your" area.
addAccelerationToken :: (ReverseQueue m) => m ()
addAccelerationToken = do
  schemes <- getMainSchemesIn . areaId =<< getYourArea
  for_ (headMay schemes) placeAccelerationToken

placeAccelerationToken :: (ReverseQueue m) => MainSchemeId -> m ()
placeAccelerationToken = push . PlaceAccelerationToken

-- | The obligation's own step, once the player may have changed form.
resolveObligation :: (ReverseQueue m) => ObligationId -> IdentityId -> m ()
resolveObligation oid = push . ObligationMessage oid . ResolveObligation

-- | The cards go on top of the encounter deck, the first on top.
putOnTopOfEncounterDeck :: (ReverseQueue m) => [EncounterCard] -> m ()
putOnTopOfEncounterDeck = push . PutOnTopOfEncounterDeck

-- | The cards go to the bottom of the encounter deck, the last given bottommost.
putOnBottomOfEncounterDeck :: (ReverseQueue m) => [EncounterCard] -> m ()
putOnBottomOfEncounterDeck = push . PutOnBottomOfEncounterDeck

dealEncounterCard :: (ReverseQueue m) => IdentityId -> m ()
dealEncounterCard = push . DealEncounterCard

-- * The Infinity Stone deck (The Mad Titan's Shadow)
--
-- The Infinity Gauntlet's six stones: a deck of their own ('InfinityStoneDeckKey') with
-- its own discard pile, shuffled back without penalty as it empties.

-- | The top stone is put into play for the player, not revealed.
putTopInfinityStoneIntoPlay :: (ReverseQueue m) => IdentityId -> m ()
putTopInfinityStoneIntoPlay you = fromDeck InfinityStoneDeckKey (push $ PutTopOfEncounterDeckIntoPlay you)

-- | The player reveals the top stone.
revealTopInfinityStone :: (ReverseQueue m) => IdentityId -> m ()
revealTopInfinityStone you = fromDeck InfinityStoneDeckKey (drawAndRevealEncounterCard you)

-- | The stone discard pile is shuffled into the stone deck.
shuffleStoneDiscardIntoDeck :: (ReverseQueue m) => m ()
shuffleStoneDiscardIntoDeck = fromDeck InfinityStoneDeckKey (push ShuffleEncounterDiscardIntoDeck)

-- | The top stones are discarded to the stone discard pile, then the body does what it
-- does with them: as many as there were.
discardTopInfinityStoneThen :: (ReverseQueue m) => Natural -> ([EncounterCard] -> m ()) -> m ()
discardTopInfinityStoneThen n body = do
  cards <- take (fromIntegral n) . pileDeck <$> getPile InfinityStoneDeckKey
  fromDeck InfinityStoneDeckKey (discardTopOfEncounterDeck n)
  body cards

-- * Game areas and their decks
--
-- What "the villain", "the main scheme" and "the encounter deck" are is "your" area's
-- ('getYourArea'); the players cannot target what is in other areas.

-- | A new game area, the players given leaving theirs for it. Its id is new, so several
-- may be made at once.
createArea :: (ReverseQueue m) => [IdentityId] -> m AreaId
createArea players = do
  -- small enough for the client to read exactly
  aid <- AreaId <$> getRandomR (1, 2 ^ (31 :: Int))
  push $ CreateArea aid players
  pure aid

-- | The player joins the area: the minions engaged with them, and the cards they
-- control, go with them.
movePlayerToArea :: (ReverseQueue m) => IdentityId -> AreaId -> m ()
movePlayerToArea ident = push . MovePlayerToArea ident

-- | Everything in the first area joins the second, and the first is gone.
combineAreas :: (ReverseQueue m) => AreaId -> AreaId -> m ()
combineAreas from = push . CombineAreas from

-- | The villain becomes his area's active villain.
setActiveVillain :: (ReverseQueue m) => VillainId -> m ()
setActiveVillain = push . SetActiveVillain

-- | The side scheme is the villain's own: he schemes onto it, it cannot leave play while
-- he is in play, and it is removed from the game when he is defeated.
setSignatureScheme :: (ReverseQueue m) => VillainId -> SideSchemeId -> m ()
setSignatureScheme vid = push . SetSignatureScheme vid

-- | A deck of its own made from the cards, shuffled, wherever they were.
createDeck :: (ReverseQueue m) => DeckKey -> [EncounterCard] -> m ()
createDeck key = push . CreateDeck key

removeDeck :: (ReverseQueue m) => DeckKey -> m ()
removeDeck = push . RemoveDeck

-- | The body's encounter-deck steps (drawing, dealing, revealing, searching,
-- discarding, shuffling) against the deck given: @fromDeck (NamedDeckKey
-- "experimentalWeapons") (drawAndRevealEncounterCard you)@.
fromDeck :: (ReverseQueue m) => DeckKey -> m () -> m ()
fromDeck key body = pushAll . map (UsingDeck key) =<< capture body

-- | The villain is defeated for good: the scenario says what follows.
resolveVillainDefeat :: (ReverseQueue m) => VillainId -> m ()
resolveVillainDefeat = push . ResolveVillainDefeat

-- * Villains with side schemes of their own (The Wrecking Crew)

data ThreatExtreme = MostThreat | LeastThreat
  deriving stock (Show, Eq)

-- | Of those scored, the ones with the most (or the least).
extremes :: ThreatExtreme -> [(a, Natural)] -> [a]
extremes _ [] = []
extremes ex scored = [a | (a, n) <- scored, n == best]
 where
  best = (if ex == MostThreat then maximum else minimum) (map snd scored)

sideSchemeThreatOf :: (HasGame m) => SideSchemeId -> m Natural
sideSchemeThreatOf = selectCount Matchers.SchemeThreat . SchemeWithId . SchemeSideSchemeId

-- | The villain's own side scheme, while he is in play.
signatureSchemeOf :: (HasGame m) => VillainId -> m (Maybe SideSchemeId)
signatureSchemeOf = selectOne . SignatureSchemeOf . VillainWithId

-- | The villain whose own the side scheme is.
signatureSchemeOwner :: (HasGame m) => SideSchemeId -> m (Maybe VillainId)
signatureSchemeOwner sid = headMay <$> (filterM (fmap (== Just sid) . signatureSchemeOf) =<< selectList AnyVillain)

-- | What the body does with one of them, the first player choosing among several tied:
-- "if there is a tie, the first player chooses" (TWC insert p.6, Breakout 1B).
chooseAmongTied :: (ReverseQueue m, IsRef a) => [a] -> (a -> m ()) -> m ()
chooseAmongTied xs body = case xs of
  [] -> pure ()
  [x] -> body x
  _ -> do
    first <- selectJust FirstPlayer
    chooseOnePrompted first "$label.breakTie" [targeting x (body x) | x <- xs]

-- | "The villain whose side scheme has the most (least) threat": of the villains with a
-- side scheme of their own.
withVillainBySideSchemeThreat :: (ReverseQueue m) => ThreatExtreme -> (VillainId -> m ()) -> m ()
withVillainBySideSchemeThreat ex body = do
  villains <- selectList AnyVillain
  scored <- fmap catMaybes $ for villains \v -> traverse (fmap (v,) . sideSchemeThreatOf) =<< signatureSchemeOf v
  chooseAmongTied (extremes ex scored) body

-- | "The side scheme with the most (least) threat", of those matching.
withSideSchemeByThreat :: (ReverseQueue m) => ThreatExtreme -> SideSchemeMatcher -> (SideSchemeId -> m ()) -> m ()
withSideSchemeByThreat ex matcher body = do
  schemes <- selectList matcher
  scored <- for schemes \sid -> (sid,) <$> sideSchemeThreatOf sid
  chooseAmongTied (extremes ex scored) body

-- | "Move the active counter to the villain whose side scheme has the most (least)
-- threat."
moveActiveCounter :: (ReverseQueue m) => ThreatExtreme -> m ()
moveActiveCounter ex = withVillainBySideSchemeThreat ex setActiveVillain

-- | The villain attacks the player, the attack given no boost card.
villainAttacksWithoutBoost :: (ReverseQueue m) => VillainId -> IdentityId -> m ()
villainAttacksWithoutBoost vid = push . VillainMessage vid . VillainAttacksWithoutBoost

-- | Threat taken off the side scheme, not by thwarting.
removeSideSchemeThreat :: (ReverseQueue m) => SideSchemeId -> Natural -> m ()
removeSideSchemeThreat sid = push . SideSchemeMessage sid . SideSchemeThreatRemoved GameRef

-- | Of the upgrades and the supports the player controls, as asked for, those with the
-- highest printed cost.
highestCostControlled :: (HasGame m) => IdentityId -> Bool -> Bool -> m [Target]
highestCostControlled you upgrades supports = do
  us <- if upgrades then selectList (UpgradeControlledBy $ IdentityWithId you) else pure []
  ss <- if supports then selectList (SupportControlledBy $ IdentityWithId you) else pure []
  targets <- filterM discardable (map UpgradeRef us <> map SupportRef ss)
  costed <- for targets \t ->
    (t,) . fromIntegral . max 0 . fromMaybe 0 . (cdCost =<<) <$> getsGame (`abilityCardDef` t)
  pure $ extremes MostThreat costed
 where
  discardable target = do
    def <- getsGame (`abilityCardDef` target)
    modifiers <- getModifiers target
    pure $ maybe True (notElem Permanent . cdKeywords) def && CannotBeDiscarded `notElem` modifiers

-- | "The highest-cost card you control": of the player's allies, supports and upgrades,
-- each with the highest printed cost.
highestCostCardsControlled :: (HasGame m) => IdentityId -> m [Target]
highestCostCardsControlled you = do
  as <- selectList (AllyControlledBy $ IdentityWithId you)
  us <- selectList (UpgradeControlledBy $ IdentityWithId you)
  ss <- selectList (SupportControlledBy $ IdentityWithId you)
  defs <- for (map AllyRef as <> map UpgradeRef us <> map SupportRef ss) \t -> (t,) <$> getsGame (`abilityCardDef` t)
  -- a permanent card cannot be discarded, so another that fits is chosen (RR Permanent)
  let costed = [(t, fromIntegral . max 0 . fromMaybe 0 $ cdCost =<< d) | (t, d) <- defs, maybe True ((Permanent `notElem`) . cdKeywords) d]
  pure $ extremes MostThreat costed

-- | A newly initiated activation waits for the current activation and every
-- response to it. The game batches simultaneous deferred activations for first
-- player ordering; with no activation underway it resolves immediately.
enemyActivationAfterCurrent :: (ReverseQueue m, IsEnemy enemy) => enemy -> m () -> m ()
enemyActivationAfterCurrent enemy body = capture body >>= cardSpecific GameRef "deferEnemyActivation" . (toEnemy enemy,)

-- | The villain's activation the body initiates during the enemy's activation under way:
-- "the newly initiated activation resolves after the current activation has finished
-- resolving", the abilities the current one triggered first, and several initiated so
-- in the order the first player decides (RR "Activation", v1.8 p.6). It follows the
-- current attack's end, or its scheming; with no activation under way, it is at once.
activationAfter :: (ReverseQueue m) => EnemyId -> VillainId -> m () -> m ()
activationAfter enemy v body = do
  msgs <- capture body
  attacking <- case enemy of
    EnemyAttachmentId _ -> pure False
    EnemyVillainId e -> isJust <$> getVillainAttack e
    EnemyMinionId _ -> pure True
  let ends = \case
        VillainMessage vid VillainEndAttack -> attacking && EnemyVillainId vid == enemy
        VillainMessage vid VillainSchemed -> not attacking && EnemyVillainId vid == enemy
        MinionMessage mid MinionEndAttack -> EnemyMinionId mid == enemy
        _ -> False
  queued <- isQueued ends
  if not queued
    then pushAll msgs
    else withQueue_ \q -> case break ends q of
      (pre, end : PendingActivations pending : rest) -> pre <> (end : PendingActivations (pending <> [(v, msgs)]) : rest)
      (pre, end : rest) -> pre <> (end : PendingActivations [(v, msgs)] : rest)
      (pre, []) -> pre

-- | The villain in play with the title, if he is.
villainNamed :: (HasGame m) => Text -> m (Maybe VillainId)
villainNamed name = headMay <$> (filterM named =<< selectList AnyVillain)
 where
  named v = getsGame \g -> maybe False ((== name) . display . cdName) (abilityCardDef g (VillainRef v))

-- | The player's upgrade or support goes to its owner's discard pile.
discardControlled :: (ReverseQueue m) => Target -> m ()
discardControlled = \case
  UpgradeRef u -> discardUpgrade u
  t -> removeFromPlay t

-- | "Discard an ally, upgrade or support you control": the player chooses one of the
-- cards given to discard. A permanent card cannot leave play, so is not offered; with
-- none left, nothing is asked.
chooseAndDiscardControlled :: (ReverseQueue m) => IdentityId -> [Target] -> m ()
chooseAndDiscardControlled you cards = do
  discardable <- filterM (\target -> do
    def <- getsGame (`abilityCardDef` target)
    modifiers <- getModifiers target
    pure $ maybe False (notElem Permanent . cdKeywords) def
      && CannotBeDiscarded `notElem` modifiers && CannotLeavePlay `notElem` modifiers) cards
  unless (null discardable) $ chooseOne you [targeting c (discardControlled c) | c <- discardable]

-- * Effects

createEffect :: (ReverseQueue m, IsRef source) => CardDef -> source -> EntityMatcher -> m ()
createEffect def source = push . CreatedEffect def (toSource source)

disableEffect :: (ReverseQueue m) => EffectId -> m ()
disableEffect eid = push $ EffectMessage eid DisableEffect

-- | The option chosen of the effect's own ('EffectChoice').
effectChoice :: (ReverseQueue m) => EffectId -> Natural -> m ()
effectChoice eid = push . EffectMessage eid . EffectChoice

-- | The disabled effect leaves the game.
effectDisabled :: (ReverseQueue m) => EffectId -> m ()
effectDisabled = push . DisabledEffect

-- | A step of the card's own, for it to answer as @CardSpecific@.
cardSpecific :: (ReverseQueue m, IsRef a, ToJSON v) => a -> Text -> v -> m ()
cardSpecific a key = push . Msg.cardSpecific a key

-- * Windows

-- | Each player in turn, from the first given, may answer the windows.
checkWindowsFor :: (ReverseQueue m) => [IdentityId] -> [Window] -> m ()
checkWindowsFor order = push . CheckWindowsFor order

-- | The windows opened are closed.
endCheckWindows :: (ReverseQueue m) => m ()
endCheckWindows = push EndCheckWindows

checkWindows :: (ReverseQueue m) => [Window] -> m ()
checkWindows = push . CheckWindows

-- * The game and the round

gameOver :: (ReverseQueue m) => FinishedStatus -> m ()
gameOver = push . GameOver

beginPhase :: (ReverseQueue m) => Phase -> m ()
beginPhase = push . BeginPhase

-- | What the body does, at the end of the phase under way.
atEndOfPhase :: (ReverseQueue m) => m () -> m ()
atEndOfPhase body = push . AtEndOfPhase =<< capture body

endPhase :: (ReverseQueue m) => Phase -> m ()
endPhase = push . EndPhase

beginRound :: (ReverseQueue m) => m ()
beginRound = push BeginRound

-- | A villain phase step begins, for the log.
villainPhaseStep :: (ReverseQueue m) => VillainPhaseStep -> m ()
villainPhaseStep = push . VillainPhaseStep

endRound :: (ReverseQueue m) => m ()
endRound = push EndRound

-- | The player whose turn it is, or who is resolving what comes next.
setActivePlayer :: (ReverseQueue m) => IdentityId -> m ()
setActivePlayer = push . SetActivePlayer

-- | The first player token goes to the next player.
passFirstPlayer :: (ReverseQueue m) => m ()
passFirstPlayer = push PassFirstPlayer

-- | The villain phase's threat on the main scheme.
accelerateMainScheme :: (ReverseQueue m) => m ()
accelerateMainScheme = push AccelerateMainScheme

-- | Each player in turn reveals the encounter cards dealt them.
revealDealtEncounterCards :: (ReverseQueue m) => m ()
revealDealtEncounterCards = push RevealDealtEncounterCards

startScenario :: (ReverseQueue m) => m ()
startScenario = push StartScenario

-- | Each player is dealt their starting hand, then the first round begins.
-- | The encounter cards with the setup keyword go into play (RR Appendix II, step 11).
putSetupEncounterCardsIntoPlay :: (ReverseQueue m) => m ()
putSetupEncounterCardsIntoPlay = push PutSetupEncounterCardsIntoPlay

setupPlayers :: (ReverseQueue m) => m ()
setupPlayers = push SetupPlayers

-- | The players' order, the first player first.
setPlayerOrder :: (ReverseQueue m) => [IdentityId] -> m ()
setPlayerOrder = push . SetPlayerOrder

-- | The main scheme's setup, as the game begins.
setupMainScheme :: (ReverseQueue m) => m ()
setupMainScheme = push SetupMainScheme

addMainScheme :: (ReverseQueue m) => CardCode -> m ()
addMainScheme = push . AddMainScheme

addVillain :: (ReverseQueue m) => CardCode -> m ()
addVillain = push . AddVillain

-- | The first main scheme, revealed as the game is set up.
addSetupMainScheme :: (ReverseQueue m) => CardCode -> m ()
addSetupMainScheme = push . AddSetupMainScheme

-- | The main scheme leaves play for the one given, which takes its place in its area.
replaceMainScheme :: (ReverseQueue m) => MainSchemeId -> CardCode -> m ()
replaceMainScheme mid = push . ReplaceMainScheme mid

-- * Campaigns

-- | The scenario follows that part of its Campaign Instructions.
campaignStep :: (ReverseQueue m) => CampaignStepKind -> m ()
campaignStep = push . CampaignStep

-- | The player reads the sections (i18n keys: the campaign's story and rules) and goes on.
readThenContinue :: (ReverseQueue m) => IdentityId -> [Text] -> m ()
readThenContinue you sections = push . Ask you . Read sections . ChooseOne . pure =<< labeled "$label.continue" (pure ())

-- | Written in the campaign log, in a campaign game; otherwise nothing.
recordInCampaignLog :: (ReverseQueue m) => CampaignRecord -> m ()
recordInCampaignLog = push . RecordInCampaignLog

-- | Written in the log for the player, who the log knows by their identity.
recordForPlayer :: (ReverseQueue m) => IdentityId -> (CardCode -> CampaignRecord) -> m ()
recordForPlayer ident f = recordInCampaignLog . f =<< getIdentityCode ident

-- | The player's card crossed out of the campaign log: not in their deck again.
removeFromCampaignLog :: (ReverseQueue m) => IdentityId -> CardCode -> m ()
removeFromCampaignLog ident code = recordForPlayer ident (`RemoveFromCampaignLog` code)

putObligationIntoPlay :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
putObligationIntoPlay ident = push . PutObligationIntoPlay ident

-- | Each player's obligations out of the encounter deck and the game.
removePlayerObligationsFromGame :: (ReverseQueue m) => m ()
removePlayerObligationsFromGame = push RemovePlayerObligationsFromGame

-- * Steps of an element's own
--
-- What an element does next that is its own business, for its runner to answer: what
-- the engine asks of it, not what a card does.

-- | A step of the villain's own, for its runner to answer.
villainStep :: (ReverseQueue m) => VillainId -> VillainMessage -> m ()
villainStep vid = push . VillainMessage vid

-- | A step of the upgrade's own, for its runner to answer.
upgradeStep :: (ReverseQueue m) => UpgradeId -> UpgradeMessage -> m ()
upgradeStep u = push . UpgradeMessage u

-- | A step of the player's own, for their identity to answer.
identityStep :: (ReverseQueue m) => IdentityId -> IdentityMessage -> m ()
identityStep ident = push . IdentityMessage ident

supportStep :: (ReverseQueue m) => SupportId -> SupportMessage -> m ()
supportStep s = push . SupportMessage s

allyStep :: (ReverseQueue m) => AllyId -> AllyMessage -> m ()
allyStep ally = push . AllyMessage ally

minionStep :: (ReverseQueue m) => MinionId -> MinionMessage -> m ()
minionStep mid = push . MinionMessage mid

treacheryStep :: (ReverseQueue m) => TreacheryId -> TreacheryMessage -> m ()
treacheryStep tid = push . TreacheryMessage tid

obligationStep :: (ReverseQueue m) => ObligationId -> ObligationMessage -> m ()
obligationStep oid = push . ObligationMessage oid

environmentStep :: (ReverseQueue m) => EnvironmentId -> EnvironmentMessage -> m ()
environmentStep eid = push . EnvironmentMessage eid

attachmentStep :: (ReverseQueue m) => AttachmentId -> AttachmentMessage -> m ()
attachmentStep aid = push . AttachmentMessage aid

sideSchemeStep :: (ReverseQueue m) => SideSchemeId -> SideSchemeMessage -> m ()
sideSchemeStep sid = push . SideSchemeMessage sid

mainSchemeStep :: (ReverseQueue m) => MainSchemeId -> MainSchemeMessage -> m ()
mainSchemeStep mid = push . MainSchemeMessage mid

eventStep :: (ReverseQueue m) => EventId -> EventMessage -> m ()
eventStep eid = push . EventMessage eid

effectStep :: (ReverseQueue m) => EffectId -> EffectMessage -> m ()
effectStep eid = push . EffectMessage eid

-- | The event's effect, unless a stun or confusion takes its place.
resolveEventEffect :: (ReverseQueue m) => IdentityId -> EventId -> EventMessage -> m ()
resolveEventEffect ident eid = push . ResolveEventEffect ident . EventMessage eid

-- | The boost card turned faceup resolves its boost ability.
revealedAsBoost :: (ReverseQueue m, IsEnemy enemy) => Target -> enemy -> m ()
revealedAsBoost target = push . RevealedAsBoost target . toEnemy

-- * Telling a card what it asked for

-- | The target is told the cards discarded from the zone.
withDiscarded :: (ReverseQueue m, IsRef target) => target -> FromZone -> [Card] -> m ()
withDiscarded target zone = push . WithDiscarded (toTarget target) zone

-- | The target is told the card discarded from the zone matched.
withDiscardedMatch :: (ReverseQueue m, IsRef target) => target -> FromZone -> Card -> m ()
withDiscardedMatch target zone = push . WithDiscardedMatch (toTarget target) zone

-- | The target is told the cards chosen from the zone.
withChosen :: (ReverseQueue m, IsRef target) => target -> FromZone -> [Card] -> m ()
withChosen target zone = push . WithChosen (toTarget target) zone

-- | The target is told the cards found.
searchFoundCards :: (ReverseQueue m, IsRef target) => target -> [Card] -> m ()
searchFoundCards target = push . SearchFoundCards (toTarget target)

-- * Interrupts: what is on the queue changed

-- | Cancel this reveal's When Revealed abilities, retaining its entry into play.
cancelEncounterWhenRevealed :: (HasQueue m, HasGame m) => EncounterCard -> m ()
cancelEncounterWhenRevealed card = do
  allowed <- encounterCardCanBeCanceled card
  when allowed $ cancelMatchingMessage $ grantedSurgeFor $ ecCardId card
  when allowed $ replaceMatchingMessage replace $ \case
    RevealedEncounterCard _ other -> ecCardId other == ecCardId card
    RevealedEncounterCardFrom _ _ other -> ecCardId other == ecCardId card
    CardSpecific GameRef "revealAfterWould" (fromJSON -> Success (_ :: W.RevealSource, _ :: IdentityId, other :: EncounterCard)) -> ecCardId other == ecCardId card
    _ -> False
 where
  replace (RevealedEncounterCardFrom source you other) = [RevealedEncounterCardWithoutWhenRevealedFrom source you other]
  replace (RevealedEncounterCard you other) = [RevealedEncounterCardWithoutWhenRevealed you other]
  replace (CardSpecific GameRef "revealAfterWould" value) = [CardSpecific GameRef "revealAfterWouldWithoutWhenRevealed" value]
  replace message = [message]

-- | Cancel the entire pending encounter card: it never enters play, and neither
-- its keywords nor its When Revealed effects resolve.
cancelEncounterCard :: (HasQueue m, HasGame m) => EncounterCard -> m ()
cancelEncounterCard card = do
  allowed <- encounterCardCanBeCanceled card
  when allowed $ cancelMatchingMessage $ grantedSurgeFor $ ecCardId card
  when allowed $ replaceMatchingMessage (const [DiscardedCard $ toCard card]) $ \case
    RevealedEncounterCard _ other -> other == card
    RevealedEncounterCardFrom _ _ other -> other == card
    RevealedEncounterCardWithoutWhenRevealed _ other -> other == card
    RevealedEncounterCardWithoutWhenRevealedFrom _ _ other -> other == card
    CardSpecific GameRef key (fromJSON -> Success (_ :: W.RevealSource, _ :: IdentityId, other :: EncounterCard)) | key `elem` ["revealAfterWould", "revealAfterWouldWithoutWhenRevealed"] -> other == card
    _ -> False

-- | The source of the damage the identity is about to take ("when you would take damage").
pendingDamageSource :: (HasQueue m) => IdentityId -> m (Maybe Source)
pendingDamageSource ident = withQueue \q -> (q, headMay [s | IdentityMessage i (IdentityDamaged s _) <- concatMap queuedMessageLeaves q, i == ident])

pendingVillainDamageSource :: (HasQueue m) => VillainId -> m (Maybe Source)
pendingVillainDamageSource vid = withQueue \q -> (q, headMay [source | VillainMessage who (VillainDamaged source _) <- concatMap queuedMessageLeaves q, who == vid])

-- | The damage the identity is about to take ("when you would take damage"), as other
-- interrupts have left it.
pendingDamage :: (HasQueue m) => IdentityId -> m (Maybe Damage)
pendingDamage ident = withQueue \q -> (q, headMay [d | IdentityMessage i (IdentityDamaged _ d) <- concatMap queuedMessageLeaves q, i == ident])

-- | The next attack damage pending against a character, after earlier prevention.
pendingAttackDamage :: (HasQueue m, IsRef character) => character -> m Natural
pendingAttackDamage host = withQueue $ \queue -> (queue, fromMaybe 0 $ headMay $ mapMaybe amount $ concatMap queuedMessageLeaves queue)
 where
  amount msg = case (toTarget host, msg) of
    (IdentityRef ident, IdentityMessage who (IdentityDamaged _ damage)) | ident == who, fromAttack damage -> Just $ damageAmount damage
    (AllyRef aid, AllyMessage who (AllyDamaged _ damage)) | aid == who, fromAttack damage -> Just $ damageAmount damage
    (CharacterRef (IdentityCharacter ident), _) -> amountFor (IdentityRef ident) msg
    (CharacterRef (AllyCharacter aid), _) -> amountFor (AllyRef aid) msg
    _ -> Nothing
  amountFor target msg = case (target, msg) of
    (IdentityRef ident, IdentityMessage who (IdentityDamaged _ damage)) | ident == who, fromAttack damage -> Just $ damageAmount damage
    (AllyRef aid, AllyMessage who (AllyDamaged _ damage)) | aid == who, fromAttack damage -> Just $ damageAmount damage
    _ -> Nothing
  fromAttack damage = case damageSource damage of
    FromAttack -> True
    FromPlayerAttack _ -> True
    FromAllyAttack _ -> True
    FromOverkill -> True
    _ -> False

-- | All of the damage the identity is about to take is cancelled.
cancelDamage :: (HasQueue m) => IdentityId -> m ()
cancelDamage ident = cancelMatchingMessage \case
  IdentityMessage ident' (IdentityDamaged _ _) -> ident' == ident
  _ -> False

-- | Cancel only this card's boost ability, preserving its icons and every other card.
cancelBoostEffect :: (HasQueue m) => EncounterCard -> m ()
cancelBoostEffect card = replaceMatchingMessage replace matchesMessage
 where
  matchesMessage = \case
    RevealBoostCard other _ -> other == card
    CardSpecific GameRef "revealBoostAfterWindow" (fromJSON -> Success (_ :: Bool, other :: EncounterCard, _ :: EnemyId)) -> other == card
    RevealedAsBoost target _ -> matchesTarget target
    _ -> False
  replace = \case
    RevealBoostCard other enemy -> [RevealBoostCardWithoutAbility other enemy]
    CardSpecific GameRef "revealBoostAfterWindow" (fromJSON -> Success (_ :: Bool, other :: EncounterCard, enemy :: EnemyId)) ->
      [CardSpecific GameRef "revealBoostAfterWindow" $ toJSON (False, other, enemy)]
    RevealedAsBoost _ _ -> []
    other -> [other]
  matchesTarget = \case
    TreacheryRef (TreacheryId uid) -> matches uid
    MinionRef (MinionId uid) -> matches uid
    AttachmentRef (AttachmentId uid) -> matches uid
    SideSchemeRef (SideSchemeId uid) -> matches uid
    ObligationRef (ObligationId uid) -> matches uid
    EnvironmentRef (EnvironmentId uid) -> matches uid
    VillainRef (VillainId uid) -> matches uid
    MainSchemeRef (MainSchemeId uid) -> matches uid
    CardIdRef cid -> cid == toCardId card
    _ -> False
  matches uid = CardId uid == toCardId card

-- | The queued damage still awaiting resolution for a friendly character.
pendingCharacterDamage :: (HasQueue m, IsCharacter character) => character -> m (Maybe (Source, Damage))
pendingCharacterDamage (toCharacter -> character) = withQueue \q -> (q, headMay $ mapMaybe matching $ concatMap queuedMessageLeaves q)
 where
  matching = \case
    IdentityMessage ident (IdentityDamaged source damage)
      | character == IdentityCharacter ident -> Just (source, damage)
    AllyMessage aid (AllyDamaged source damage)
      | character == AllyCharacter aid -> Just (source, damage)
    _ -> Nothing

-- | The queued attack damage still awaiting resolution for a friendly character.
pendingCharacterAttackDamage :: (HasQueue m, IsCharacter character) => character -> m (Maybe (Source, Damage))
pendingCharacterAttackDamage (toCharacter -> character) = withQueue \q -> (q, headMay $ mapMaybe matching q)
 where
  matching = \case
    IdentityMessage ident (IdentityDamaged source damage)
      | character == IdentityCharacter ident && damageSource damage == FromAttack -> Just (source, damage)
    AllyMessage aid (AllyDamaged source damage)
      | character == AllyCharacter aid && damageSource damage == FromAttack -> Just (source, damage)
    _ -> Nothing

-- | Cancel the pending attack damage to this friendly character.
cancelCharacterAttackDamage :: (HasQueue m, IsCharacter character) => character -> m ()
cancelCharacterAttackDamage (toCharacter -> character) = cancelMatchingMessage \case
  IdentityMessage ident (IdentityDamaged _ damage) -> character == IdentityCharacter ident && damageSource damage == FromAttack
  AllyMessage aid (AllyDamaged _ damage) -> character == AllyCharacter aid && damageSource damage == FromAttack
  _ -> False

-- | Replace turning this facedown boost card faceup with discarding it. Keep every
-- other boost card, including any other cards in a pending icon count.
discardBoostCardBeforeReveal :: (ReverseQueue m) => EncounterCard -> m ()
discardBoostCardBeforeReveal card = do
  cancelBoostIconsOnCard card
  replaceMatchingMessage removeCard \case
    RevealBoostCard other _ -> other == card
    RevealBoostCardWithoutAbility other _ -> other == card
    CardSpecific GameRef "revealBoostAfterWindow" (fromJSON -> Success (_ :: Bool, other :: EncounterCard, _ :: EnemyId)) -> other == card
    _ -> False
  discardCard card
 where
  removeCard = \case
    RevealBoostCard _ _ -> []
    RevealBoostCardWithoutAbility _ _ -> []
    CardSpecific GameRef "revealBoostAfterWindow" _ -> []
    other -> [other]

-- | The boost icons about to be counted and added to the enemy's activation are
-- cancelled.
cancelBoostIcons :: (HasQueue m, IsEnemy enemy) => enemy -> m ()
cancelBoostIcons (toEnemy -> enemy) = cancelMatchingMessage \case
  CountBoostIcons ref _ _ -> isEnemy ref
  CountActivationBoostIcons ref _ -> isEnemy ref
  WithBoostIcons ref _ _ -> isEnemy ref
  VillainMessage vid (VillainAddBoost _) -> enemy == EnemyVillainId vid
  MinionMessage mid (MinionAddBoost _) -> enemy == EnemyMinionId mid
  _ -> False
 where
  isEnemy = \case
    VillainRef vid -> enemy == EnemyVillainId vid
    MinionRef mid -> enemy == EnemyMinionId mid
    _ -> False


-- | Cancel the icons of one physical boost card, leaving every other boost card
-- and this card's star ability intact (Northstar).
cancelBoostIconsOnCard :: ReverseQueue m => EncounterCard -> m ()
cancelBoostIconsOnCard card = do
  cardSpecific GameRef "cancelBoostIconsOnCard" card
  replaceMatchingMessage change matchesMessage
 where
  matchesMessage = \case
    CountActivationBoostIcons _ cards -> card `elem` cards
    CountBoostIcons _ _ cards -> card `elem` cards
    WithBoostIcons _ _ counts -> any ((== card) . fst) counts
    _ -> False
  change = \case
    CountActivationBoostIcons target cards -> [CountActivationBoostIcons target remaining | let remaining = filter (/= card) cards, notNull remaining]
    CountBoostIcons target value cards -> [CountBoostIcons target value remaining | let remaining = filter (/= card) cards, notNull remaining]
    WithBoostIcons target value counts -> [WithBoostIcons target value remaining | let remaining = filter ((/= card) . fst) counts, notNull remaining]
    other -> [other]

-- | The count of the boost icons on the card, as its window answers it ("count ...
-- instead", "increase or decrease ... for this count"), is changed.
changeBoostIconCount :: (HasQueue m) => EncounterCard -> (Natural -> Natural) -> m ()
changeBoostIconCount card f = replaceMatchingMessage change \case
  WithBoostIcons _ _ counts -> any ((== card) . fst) counts
  _ -> False
 where
  change = \case
    WithBoostIcons t v counts -> [WithBoostIcons t v [(c, if c == card then f n else n) | (c, n) <- counts]]
    msg -> [msg]

-- | A thwart's actual removal, distinguished from moving or otherwise removing threat.
thwartRemovesThreat :: (ReverseQueue m) => WindowTiming -> Source -> SchemeId -> Natural -> Natural -> m ()
thwartRemovesThreat timing source scheme before n =
  checkWindows [Window timing $ W.ThwartRemovedThreat source scheme before (min before n)]

-- | Positive-to-zero threat removal by an identity or a card that extends it.
heroRemovedLastThreat :: (ReverseQueue m) => Source -> SchemeId -> Natural -> Natural -> m ()
heroRemovedLastThreat source scheme before n = do
  let removed = min before n
  when (removed > 0) $ checkWindows [Window After $ W.ThreatRemovedBy source scheme before removed]
  when (before > 0 && n >= before) $
    getHeroSourceController source >>= traverse_ (\ident -> checkWindows [Window After $ W.LastThreatRemoved ident scheme])

-- | The window after a hero's thwart took the last threat off the scheme ("after your
-- hero thwarts and removes all threat from a scheme"): an ally's thwart is not one.
heroThwartedAllThreat :: (ReverseQueue m) => Source -> SchemeId -> Natural -> Natural -> m ()
heroThwartedAllThreat source scheme before n = case source of
  AllyRef _ -> pure ()
  _ | before == 0 || n < before -> pure ()
  _ -> getSourceController source >>= traverse_ \ident -> checkWindows [Window After $ W.ThwartedAllThreat ident scheme]

-- | The count of the boost icons on the card, as its window has it so far.
getBoostIconCount :: (HasQueue m) => EncounterCard -> m (Maybe Natural)
getBoostIconCount card = do
  withQueue \queue -> (queue, headMay [n | WithBoostIcons _ _ counts <- queue, (c, n) <- counts, c == card])


-- | Delayed damage after a defended attack shares priority with retaliate. Bundle
-- every such effect into one ordering question before defense response windows.
registerPostAttackDamage :: (ReverseQueue m, IsRef source) => IdentityId -> EnemyId -> Natural -> source -> Natural -> m ()
registerPostAttackDamage you enemy before source n = do
  bundled <- isQueued bundleMatches
  if bundled then replaceMatchingMessage appendEffect bundleMatches else do
    retaliates <- isQueued retaliationMatches
    let bundle = Msg.cardSpecific GameRef "ncAttackCompletion" (you, enemy, before, retaliates, [(toSource source, n)])
    if retaliates
      then replaceMatchingMessage (const [bundle]) retaliationMatches
      else replaceMatchingMessage (\msg -> [bundle,msg]) defendedMatches
 where
  bundleMatches = \case
    CardSpecific GameRef "ncAttackCompletion" (fromJSON -> Success (who :: IdentityId, attacker :: EnemyId, _ :: Natural, _ :: Bool, _ :: [(Source,Natural)])) -> who == you && attacker == enemy
    _ -> False
  appendEffect = \case
    CardSpecific GameRef "ncAttackCompletion" (fromJSON -> Success (who :: IdentityId, attacker :: EnemyId, previous :: Natural, retaliation :: Bool, effects :: [(Source,Natural)])) ->
      [Msg.cardSpecific GameRef "ncAttackCompletion" (who, attacker, previous, retaliation, effects <> [(toSource source,n)])]
    msg -> [msg]
  retaliationMatches = \case
    IdentityMessage who (IdentityRetaliate attacker) -> who == you && attacker == enemy
    _ -> False
  defendedMatches = \case
    IdentityMessage who (AttackDefended attacker _ _) -> who == you && attacker == enemy
    _ -> False

-- | The ally's attack about to land gets more ATK, as an interrupt to its attacking:
-- "it gets +3 ATK for that attack".
raiseAllyAttack :: (HasQueue m) => AllyId -> Natural -> m ()
raiseAllyAttack aid n = replaceMatchingMessage raise \case
  AllyMessage aid' (AllyAttackLands _ _) -> aid' == aid
  AllyMessage aid' (AllyEvenAttackLands _ _) -> aid' == aid
  _ -> False
 where
  raise = \case
    AllyMessage a (AllyAttackLands enemy dmg) -> [AllyMessage a $ AllyAttackLands enemy dmg {damageAmount = damageAmount dmg + n}]
    AllyMessage a (AllyEvenAttackLands dmg shares) -> [AllyMessage a $ AllyEvenAttackLands dmg {damageAmount = damageAmount dmg + n} shares]
    msg -> [msg]

-- | The ally's attack about to land gains overkill: "that attack gains overkill".
allyAttackGainsOverkill :: (HasQueue m) => AllyId -> m ()
allyAttackGainsOverkill aid = replaceMatchingMessage gain \case
  AllyMessage aid' (AllyAttackLands _ _) -> aid' == aid
  AllyMessage aid' (AllyEvenAttackLands _ _) -> aid' == aid
  _ -> False
 where
  gain = \case
    AllyMessage a (AllyAttackLands enemy dmg) -> [AllyMessage a $ AllyAttackLands enemy (withOverkill dmg)]
    AllyMessage a (AllyEvenAttackLands dmg shares) -> [AllyMessage a $ AllyEvenAttackLands (withOverkill dmg) shares]
    msg -> [msg]

-- | The excess damage a hero's attack is dealing is more, as an interrupt to it: "increase
-- that amount by 1". A minion's overkill and the windows after its defeat count it, as
-- does the window after a villain's.
increaseExcessDamage :: (HasQueue m) => Natural -> m ()
increaseExcessDamage n = replaceMatchingMessage raise \case
  MinionMessage _ MinionDefeatedWithExcess {} -> True
  CheckWindows ws -> any (\case Window After W.DefeatedVillain {} -> True; _ -> False) ws
  _ -> False
 where
  raise = \case
    MinionMessage m (MinionDefeatedWithExcess s d k) -> [MinionMessage m $ MinionDefeatedWithExcess s d (k + n)]
    CheckWindows ws -> [CheckWindows $ map (\case
      Window After (W.DefeatedVillain v d s k) -> Window After (W.DefeatedVillain v d s (k + n))
      window -> window) ws]
    msg -> [msg]

-- | The first message queued matching is replaced by what the body does.
insteadOf :: (ReverseQueue m) => (Message -> Bool) -> m () -> m ()
insteadOf matcher body = do
  msgs <- capture body
  replaceMatchingMessage (const msgs) matcher

-- | What the body does follows the first message queued matching.
afterMatching :: (ReverseQueue m) => (Message -> Bool) -> m () -> m ()
afterMatching matcher body = do
  msgs <- capture body
  replaceMatchingMessage (: msgs) matcher


-- | Teamwork from a minion revealed by a When Defeated ability checks the board
-- only after the defeated minion has left play (Shadowland FAQ, p.18).
afterPendingMinionDefeat :: ReverseQueue m => m () -> m ()
afterPendingMinionDefeat body = do
  pending <- withQueue $ \queue -> (queue, headMay [mid | MinionMessage mid MinionDefeated <- concatMap queuedMessageLeaves queue])
  case pending of
    Nothing -> body
    Just mid -> afterMatching (== MinionMessage mid MinionDefeated) body


-- | Find Rogue's physical Touched card, wherever it currently is, and move it.
findAndAttachTouched :: (ReverseQueue m) => IdentityId -> CharacterId -> m ()
findAndAttachTouched you host = cardSpecific GameRef "findAndAttachTouched" (you, host)

setAsideTouched :: (ReverseQueue m) => IdentityId -> m ()
setAsideTouched you = cardSpecific GameRef "setAsideTouched" you

-- | A named control effect only ends the conversion it created, not a newer one.
takeControlOfMinionFrom :: (ReverseQueue m, IsRef source) => source -> IdentityId -> MinionId -> Natural -> m ()
takeControlOfMinionFrom source you mid n = do
  takeControlOfMinion you mid
  cardSpecific (AllyRef $ AllyId $ unMinionId mid) "controlledMinionSource" (toSource source, n)

-- | Karma has two consequential damage; Mind Control keeps its printed one.
takeControlOfMinionWithConsequential :: (ReverseQueue m) => IdentityId -> MinionId -> Natural -> m ()
takeControlOfMinionWithConsequential you mid n = do
  takeControlOfMinion you mid
  cardSpecific (AllyRef $ AllyId $ unMinionId mid) "controlledMinionConsequential" n

-- | Find an owned card in any normal player zone or the set-aside zone and set it
-- aside. Identity Setup abilities may call this after the opening hand/mulligan.
setAsideOwnedCard :: (ReverseQueue m, Projection m PlayerIdentity) => IdentityId -> CardCode -> m ()
setAsideOwnedCard ident code = do
  hand <- projectMap PlayerIdentityHand unHand ident
  deck <- projectMap PlayerIdentityDeck unDeck ident
  discard <- projectMap PlayerIdentityDiscard unDiscard ident
  aside <- getSetAsidePlayerCards
  for_ (headMay [c | c <- hand <> deck <> discard <> aside, toCardCode c == code]) $ setAside . pure . toCard

-- | Begin playing a set-aside card, paying its normal costs and choosing its normal
-- targets. Callers use AffordableCardBy + InSetAsideOf as their ability criteria.
playCardFromSetAside :: (ReverseQueue m) => IdentityId -> CardCode -> m ()
playCardFromSetAside ident code = do
  cards <- getSetAsidePlayerCards
  for_ (headMay [c | c <- cards, toCardCode c == code, pcOwner c == Just ident]) $ \card -> identityStep ident (PlayingCard card Nothing)

-- | Put an owned permanent card directly into play during setup.
putCardFromSetAsideIntoPlay :: (ReverseQueue m) => IdentityId -> CardCode -> m ()
putCardFromSetAsideIntoPlay ident code = do
  cards <- getSetAsidePlayerCards
  for_ (headMay [c | c <- cards, toCardCode c == code, pcOwner c == Just ident]) $ putIntoPlay ident

-- | Recruit a neutral linked ally from the shared set-aside area.
recruitLinkedAlly :: ReverseQueue m => IdentityId -> CardCode -> m ()
recruitLinkedAlly you code = do
  cards <- getSetAsidePlayerCards
  for_ (headMay [c | c <- cards, toCardCode c == code, isNothing (pcOwner c)]) $ \card ->
    putIntoPlay you card {pcOwner = Just you, pcController = Just you}


-- | Snapshot the upgrades attached to an enemy before its defeat interrupts run.
-- Pass the same snapshot to the When and After windows.
enemyDefeatUpgrades :: (HasGame m) => EnemyId -> m [CardDef]
enemyDefeatUpgrades enemy = do
  upgrades <- selectList (UpgradeMatches [])
  catMaybes <$> for upgrades (\uid -> do
    let matches = EnemyWithId enemy <> EnemyWithUpgrade (UpgradeWithId uid)
    found <- not . null <$> gameSelectEnemy matches
    if found then getsGame (`abilityCardDef` UpgradeRef uid) else pure Nothing)

-- | Newly attacked targets answer within the original attack's interrupt window,
-- so the same attack-expanding interrupt cannot be used a second time. One attack
-- then lands against every target before its retaliate and consequential damage.
resolveAllyAttackAgainstAll :: (ReverseQueue m) => AllyId -> [EnemyId] -> m ()
resolveAllyAttackAgainstAll aid enemies = do
  original <- withQueue $ \queue -> (queue, headMay [enemy | AllyMessage who (AllyAttackLands enemy _) <- queue, who == aid])
  for_ original $ \target -> cardSpecific GameRef "checkWindowsInCurrentOccurrence" [Window When $ W.AllyAttack aid enemy | enemy <- enemies, enemy /= target]
  replaceMatchingMessage replaceAttack \case
    AllyMessage who (AllyAttackLands _ _) -> who == aid
    _ -> False
 where
  replaceAttack = \case
    AllyMessage who (AllyAttackLands _ damage) -> [AllyMessage who $ AllyAttackMultiple enemies damage]
    msg -> [msg]

-- | Put an upgrade into play attached to the chosen target, without playing it.
putIntoPlayOn :: (ReverseQueue m) => IdentityId -> PlayerCard -> Target -> m ()
putIntoPlayOn ident card target = push $ PutCardIntoPlay ident card (TargetPayment target) Nothing False

-- | Finish a cost's already chosen continuation without initiating a new ability.
enqueueContinuation :: (ReverseQueue m) => [Message] -> m ()
enqueueContinuation = pushAll

payCostThen :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Cost -> [Message] -> m ()
payCostThen you source cost continuation = do
  acid <- getRandom
  setActiveCost $ ActiveCost acid you (ForContinuation (toSource source) continuation) cost NoPayment Nothing mempty Nothing

-- | A paid continuation that needs the receipt, without initiating another
-- ability or keeping its payment open while its effects resolve.
payCostThenWithPayment :: (ReverseQueue m, IsRef source, IsRef target, ToJSON value) => IdentityId -> source -> Cost -> target -> Text -> value -> m ()
payCostThenWithPayment you source cost target key value = do
  acid <- getRandom
  setActiveCost $ ActiveCost acid you (ForPaymentContinuation (toSource source) (toTarget target) key (toJSON value)) cost NoPayment Nothing mempty Nothing

-- | A permanent upgrade returns to its controller's play area when its host leaves.
detachUpgrade :: (ReverseQueue m) => UpgradeId -> m ()
detachUpgrade uid = upgradeStep uid UpgradeDetached

redirectVillainAttackToAlly :: (ReverseQueue m) => VillainId -> AllyId -> m ()
redirectVillainAttackToAlly vid aid = do
  cancelMatchingMessage $ \case
    DeclareDefense _ (EnemyVillainId target) _ -> target == vid
    _ -> False
  cardSpecific GameRef "redirectVillainAttackToAlly" (vid, aid)

-- | Standard-set activations use the Sinister Six's counter and ambush rules.
currentVillainAttacks :: (ReverseQueue m) => IdentityId -> m ()
currentVillainAttacks you = do
  code <- getScenarioCode
  if code == "27100" then cardSpecific GameRef "sinisterSixActivate" (you, True)
    else selectOne Matchers.ActiveVillain >>= traverse_ (`enemyAttacks` you)

-- | An activation's additional boost cards follow any Ambush that brings its villain into play.
currentVillainAttacksWithAdditionalBoostCards :: (ReverseQueue m) => IdentityId -> Natural -> m ()
currentVillainAttacksWithAdditionalBoostCards you n = do
  code <- getScenarioCode
  if code == "27100" then cardSpecific GameRef "sinisterSixActivateWithBoost" (you, n)
    else selectOne Matchers.ActiveVillain >>= traverse_ (\vid -> villainStep vid $ VillainAttacksWithAdditionalBoostCards you n)

currentVillainSchemes :: (ReverseQueue m) => IdentityId -> m ()
currentVillainSchemes you = do
  code <- getScenarioCode
  if code == "27100" then cardSpecific GameRef "sinisterSixActivate" (you, False)
    else selectOne Matchers.ActiveVillain >>= traverse_ (`villainSchemesAgainst` you)

-- | Change the current attack's damage type without beginning another activation.
makeAttackIndirect :: (ReverseQueue m, IsEnemy enemy) => enemy -> m ()
makeAttackIndirect enemy = case toEnemy enemy of
  EnemyVillainId vid -> villainStep vid VillainAttackBecomesIndirect
  EnemyMinionId mid -> minionStep mid MinionAttackBecomesIndirect
  EnemyAttachmentId _ -> pure ()

-- | Faceup player cards on a minion, discarded with it when it leaves play.
attachPlayerCardToMinion :: (ReverseQueue m) => MinionId -> PlayerCard -> m ()
attachPlayerCardToMinion mid = push . AttachPlayerCardToMinion mid

attachTreacheryToAlly :: (ReverseQueue m) => AllyId -> EncounterCard -> m ()
attachTreacheryToAlly aid = push . AttachTreacheryToAlly aid

makeAttackDividedEvenly :: (ReverseQueue m) => MinionId -> m ()
makeAttackDividedEvenly mid = minionStep mid MinionAttackBecomesDividedEvenly

divideAttackDamage :: (ReverseQueue m) => IdentityId -> Source -> Damage -> m ()
divideAttackDamage you source = push . DivideAttackDamage you source

dealIndirectDamageAndExhaustDamaged :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> m ()
dealIndirectDamageAndExhaustDamaged you source n = push $ IndirectDamageAndExhaust you (toSource source) (toDamage n FromAbility) []

-- Insert into Marvel.Message.Lifted; all callers refer to the same generic zone protocol.
placePlayerCardUnder :: (ReverseQueue m, IsRef target) => target -> PlayerCard -> m ()
placePlayerCardUnder target card = do
  removeFromGame (CardIdRef $ pcCardId card)
  cardSpecific target "mgHoldCard" card

placeEncounterCardUnderMainScheme :: ReverseQueue m => MainSchemeId -> EncounterCard -> m ()
placeEncounterCardUnderMainScheme sid card = do
  removeFromGame card
  cardSpecific sid "holdEncounterCard" card

-- | Tuck an out-of-play encounter card faceup, preserving its physical identity.
placeEncounterCardUnderSideScheme :: ReverseQueue m => SideSchemeId -> EncounterCard -> m ()
placeEncounterCardUnderSideScheme sid card = do
  removeFromGame card
  cardSpecific GameRef "removeEncounterCardFromPiles" card
  cardSpecific sid "holdEncounterCard" card

revealEncounterCardUnderSideScheme :: ReverseQueue m => SideSchemeId -> IdentityId -> EncounterCard -> m ()
revealEncounterCardUnderSideScheme sid you card = do
  cardSpecific sid "removeHeldEncounterCard" (ecCardId card)
  revealEncounterCardFrom W.RevealedFromSetAside you card

putTopCardsUnder :: (ReverseQueue m, IsRef target) => IdentityId -> target -> Natural -> m ()
putTopCardsUnder you target n = do
  cards <- take (fromIntegral n) <$> projectMap PlayerIdentityDeck unDeck you
  traverse_ (placePlayerCardUnder target) cards

heldCardsAt :: (HasGame m, IsRef target) => target -> m [PlayerCard]
heldCardsAt = getZoneHeldCards . toTarget

transferHeldCards :: (ReverseQueue m, IsRef source, IsRef target) => source -> target -> m ()
transferHeldCards source target = do
  cards <- heldCardsAt source
  cardSpecific source "mgClearHeld" ()
  for_ cards (cardSpecific target "mgHoldCard")

takeHeldCardIntoPlay :: (ReverseQueue m, IsRef target) => IdentityId -> target -> PlayerCard -> m ()
takeHeldCardIntoPlay you target card = do
  cardSpecific target "mgRemoveHeld" (pcCardId card)
  putIntoPlay you card

shuffleMinionIntoEncounterDeck :: (ReverseQueue m) => MinionId -> m ()
shuffleMinionIntoEncounterDeck mid = cardSpecific GameRef "shuffleMinionIntoEncounterDeck" mid

-- | Replacement of the damage this particular attack would deal, after defense.
replaceAttackDamageWithThreatRemoval :: (ReverseQueue m, IsEnemy enemy) => enemy -> IdentityId -> m ()
replaceAttackDamageWithThreatRemoval enemy you = cardSpecific GameRef "mgAttackThreatReplacement" (toEnemy enemy, you)

preventEnemyAttackDamage :: (ReverseQueue m, IsEnemy enemy) => enemy -> Natural -> m ()
preventEnemyAttackDamage enemy n = cardSpecific GameRef "mgPartialAttackPrevention" (toEnemy enemy, n)

-- | Limit the damage this identity can take from the attack currently resolving.
capEnemyAttackDamage :: ReverseQueue m => EnemyId -> IdentityId -> Natural -> m ()
capEnemyAttackDamage enemy you n = cardSpecific GameRef "aoaCapAttackDamage" (enemy, you, n)

-- | Limit the actual damage an enemy target takes from this single attack.
capAttackDamageToEnemy :: ReverseQueue m => EnemyId -> EnemyId -> Natural -> m ()
capAttackDamageToEnemy attacker target amount = cardSpecific GameRef "capAttackDamageToEnemy" (attacker,target,amount)


-- | Turn the same physical side scheme into its attachment face without discarding it.
transformSideSchemeToAttachment :: ReverseQueue m => SideSchemeId -> CardCode -> IdentityId -> m ()
transformSideSchemeToAttachment sid code you = do
  cancelMatchingMessage (== DefeatedLeavesPlay (SideSchemeRef sid))
  cardSpecific GameRef "aoaTransformSchemeToAttachment" (sid, code, you)

-- | Increase the enemy's current activation, whether it is attacking or scheming.
enemyActivationBonus :: (ReverseQueue m, IsEnemy enemy) => enemy -> Natural -> m ()
enemyActivationBonus enemy n = case toEnemy enemy of
  EnemyVillainId vid -> villainStep vid (VillainActivationBonus n)
  EnemyMinionId mid -> minionStep mid (MinionAddBoost n)
  EnemyAttachmentId _ -> pure ()

-- | The source of the attack whose damage is waiting behind its interrupt window.
currentPlayerAttackSource :: (ReverseQueue m) => IdentityId -> m (Maybe Source)
currentPlayerAttackSource you = withQueue $ \q -> (q, headMay $ concatMap attackSource $ concatMap queuedMessageLeaves q)
 where
  attackSource = \case
    PrepareEnemyDamage original -> attackSource original
    ResolveEnemyDamage original -> attackSource original
    VillainMessage _ (VillainDamaged source damage) | damageSource damage == FromPlayerAttack you -> [source]
    MinionMessage _ (MinionDamaged source damage) | damageSource damage == FromPlayerAttack you -> [source]
    CardSpecific GameRef "psylockeRedirectedAttackDamage" (fromJSON -> Success (who :: IdentityId, source :: Source, _ :: CharacterId, _ :: Damage)) | who == you -> [source]
    EventMessage eid (EventAttacked attacker _ _) | attacker == you -> [EventRef eid]
    Simultaneously messages -> concatMap attackSource messages
    _ -> []

-- | A lasting prevention effect for this attack, including all of its targets.
preventAllDamageFromAttack :: ReverseQueue m => Source -> m ()
preventAllDamageFromAttack = cardSpecific GameRef "preventAllDamageFromAttack"

-- | The current attack's pending damage, after its attack window has opened.
modifyCurrentPlayerAttack :: (ReverseQueue m) => IdentityId -> Natural -> [Keyword] -> m ()
modifyCurrentPlayerAttack you n keywords = do
  current <- currentPlayerAttackSource you
  for_ current $ \source -> withQueue_ (map $ change source)
 where
  improve d = foldr apply (d {damageAmount = damageAmount d + n}) keywords
  apply Overkill d = d {damageOverkill = True}
  apply Piercing d = d {damagePiercing = True}
  apply Ranged d = d {damageRanged = True}
  apply _ d = d
  change current = \case
    WithSimultaneousAttackContext cursor message -> WithSimultaneousAttackContext cursor (change current message)
    PrepareEnemyDamageGroup messages cursor -> PrepareEnemyDamageGroup (map (change current) messages) cursor
    TakeEnemyDamageGroup messages cursor deferred -> TakeEnemyDamageGroup (map (change current) messages) cursor deferred
    ResolveEnemyDamageGroup messages cursor deferred -> ResolveEnemyDamageGroup (map (change current) messages) cursor deferred
    PrepareEnemyDamage original -> PrepareEnemyDamage (change current original)
    ResolveEnemyDamage original -> ResolveEnemyDamage (change current original)
    VillainMessage v (VillainDamaged source d) | source == current && damageSource d == FromPlayerAttack you -> VillainMessage v (VillainDamaged source $ improve d)
    MinionMessage m (MinionDamaged source d) | source == current && damageSource d == FromPlayerAttack you -> MinionMessage m (MinionDamaged source $ improve d)
    CardSpecific GameRef "psylockeRedirectedAttackDamage" (fromJSON -> Success (who :: IdentityId, source :: Source, friend :: CharacterId, d :: Damage)) | who == you && source == current ->
      CardSpecific GameRef "psylockeRedirectedAttackDamage" (toJSON (who, source, friend, improve d))
    CardSpecific GameRef "psylockeFriendlyAttackResolved" (fromJSON -> Success (who :: IdentityId, source :: Source, friend :: CharacterId, ranged :: Bool)) | who == you && source == current ->
      CardSpecific GameRef "psylockeFriendlyAttackResolved" (toJSON (who, source, friend, ranged || Ranged `elem` keywords))
    CardSpecific GameRef "playerAttackDamageBeforeTarget" (fromJSON -> Success (who :: IdentityId, source :: Source, enemy :: EnemyId, d :: Damage)) | who == you && source == current ->
      CardSpecific GameRef "playerAttackDamageBeforeTarget" (toJSON (who, source, enemy, improve d))
    Simultaneously messages -> Simultaneously (map (change current) messages)
    CheckWindows windows | any (fromCurrent current) windows -> CheckWindows (map (improveWindow current) windows)
    message -> message
  fromCurrent current window = case windowType window of
    W.VillainDamageFrom _ source d -> source == current && damageSource d == FromPlayerAttack you
    _ -> False
  improveWindow current window = window {windowType = case windowType window of
    W.DamagedVillain v d | damageSource d == FromPlayerAttack you -> W.DamagedVillain v (improve d)
    W.VillainDamageFrom v source d | source == current && damageSource d == FromPlayerAttack you -> W.VillainDamageFrom v source (improve d)
    other -> other}

healDivided :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> CharacterMatcher -> m ()
healDivided you source n matcher = cardSpecific GameRef "mgDivideHealing" (you, toSource source, n, matcher)

removeThreatDivided :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> SchemeMatcher -> m ()
removeThreatDivided you source n matcher = cardSpecific GameRef "mgDivideRemoval" (you, toSource source, n, matcher)

-- | Heal from the named source, retaining the distinction between player and encounter effects.
healFrom :: (ReverseQueue m, IsRef source, IsCharacter character) => source -> character -> Natural -> m ()
healFrom source character n = do
  playerCard <- isPlayerCardSource (toSource source)
  if playerCard then heal character n else case toCharacter character of
    IdentityCharacter ident -> identityStep ident (IdentityHealed n)
    AllyCharacter aid -> allyStep aid (AllyHealed n)
    MinionCharacter mid -> minionStep mid (MinionHealed n)
    AttachmentCharacter _ -> pure ()
    VillainCharacter vid -> villainStep vid (VillainHealed n)

-- | Wrapped scheme, enemy and character IDs name the same element as their concrete IDs.
counterTarget :: IsRef a => a -> Target
counterTarget a = case toTarget a of
  SchemeRef scheme -> schemeRef scheme
  EnemyRef enemy -> enemyRef enemy
  CharacterRef character -> characterRef character
  target -> target

-- Phoenix Force's power counters use the existing persistent upgrade counters.
phoenixForceFor :: HasGame m => IdentityId -> m (Maybe UpgradeId)
phoenixForceFor you = selectOne (UpgradeNamed "Phoenix Force" <> UpgradeControlledBy (IdentityWithId you))

addPhoenixPower :: ReverseQueue m => IdentityId -> Natural -> m ()
addPhoenixPower you n = phoenixForceFor you >>= traverse_ (\uid -> addUpgradeUses uid n)

removePhoenixPower :: ReverseQueue m => IdentityId -> Natural -> m ()
removePhoenixPower you n = phoenixForceFor you >>= traverse_ (\uid -> removeCounters uid n)

removeAllPhoenixPower :: ReverseQueue m => IdentityId -> m ()
removeAllPhoenixPower you = do
  force <- phoenixForceFor you
  for_ force \uid -> getCountersOn (UpgradeRef uid) >>= removeCounters uid

putPhoenixForceIntoPlay :: ReverseQueue m => IdentityId -> m ()
putPhoenixForceIntoPlay you = do
  cards <- filter (\c -> toCardCode c == "34002a" && pcOwner c == Just you) <$> getSetAsidePlayerCards
  for_ cards \c -> do
    removeFromGame (CardIdRef $ pcCardId c)
    putIntoPlay you c
    addUpgradeUses (UpgradeId $ unCardId $ pcCardId c) 4

villainSchemeRemovesThreat :: (ReverseQueue m, IsRef source) => VillainId -> IdentityId -> source -> m ()
villainSchemeRemovesThreat vid you source = villainStep vid (VillainSchemeRemovesThreat you $ toSource source)

redirectAttackDamage :: (ReverseQueue m, IsEnemy enemy, IsEnemy other) => enemy -> other -> m ()
redirectAttackDamage enemy other = case toEnemy enemy of
  EnemyVillainId vid -> villainStep vid (VillainAttackDamageRedirected $ toEnemy other)
  EnemyMinionId mid -> minionStep mid (MinionAttackDamageRedirected $ toEnemy other)
  EnemyAttachmentId _ -> pure ()

minionSchemesAgainst :: ReverseQueue m => MinionId -> IdentityId -> m ()
minionSchemesAgainst mid = minionStep mid . MinionSchemesAgainst

searchEverywhereAndRevealEncounterCard :: ReverseQueue m => IdentityId -> CardMatcher -> m ()
searchEverywhereAndRevealEncounterCard you = push . SearchEverywhereAndRevealEncounterCard you

-- | Moving threat removes exactly the moved amount without thwarting, then places
-- that amount through the ordinary placement window (RR Move). It cannot duplicate
-- threat when removal is prohibited, and removal bonuses never increase a move.

moveSchemeThreat :: (ReverseQueue m, IsRef source) => source -> SchemeId -> SchemeId -> Natural -> m ()
moveSchemeThreat source from to n = when (from /= to && n > 0) do
  removable <- canMoveSchemeThreatFrom source from
  destinationExists <- not . null <$> selectList (SchemeWithId to)
  when (removable && destinationExists) do
    available <- selectCount Matchers.SchemeThreat (SchemeWithId from)
    let amount = min n available
    when (amount > 0) do
      case from of
        SchemeMainSchemeId sid -> mainSchemeStep sid (MainSchemeThreatRemoved (toSource source) amount)
        SchemeSideSchemeId sid -> sideSchemeStep sid (SideSchemeThreatRemoved (toSource source) amount)
      redirected <- elem RemovedThreatPlacedOnMinion <$> getModifiers (schemeRef from)
      unless redirected $ placeThreat to amount

-- The replacement is resolved as the scheme resolves; retain the event, its thwart
-- context and resource effects until the resulting removal has finished.
deferEventResolutionUntilScheme :: ReverseQueue m => EventId -> VillainId -> m ()
deferEventResolutionUntilScheme eid vid = withQueue_ $ \queue ->
  let completion = filter isCompletion queue
      rest = filter (not . isCompletion) queue
      insertAfter [] = completion
      insertAfter (msg : msgs)
        | msg == VillainMessage vid VillainSchemed = msg : completion <> msgs
        | otherwise = msg : insertAfter msgs
   in insertAfter rest
 where
  isCompletion (EventMessage target ResolvedEvent) = target == eid
  isCompletion (CheckWindows [Window After (W.EventPlayed _ target _)]) = target == eid
  isCompletion _ = False

-- | Place threat on a character, as Mojo's encounter cards do.
placeCharacterThreat :: (ReverseQueue m, IsCharacter c) => c -> Natural -> m ()
placeCharacterThreat c = push . PlaceCharacterThreat (toCharacter c)

removeCharacterThreat :: (ReverseQueue m, IsCharacter c) => c -> Natural -> m ()
removeCharacterThreat c = push . RemoveCharacterThreat (toCharacter c)

moveCharacterThreatToMainScheme :: (ReverseQueue m, IsCharacter c) => c -> m ()
moveCharacterThreatToMainScheme = push . MoveCharacterThreatToMainScheme . toCharacter

-- | Med Lab starts an ordinary paid play of its held ally; the reservation in the
-- payment records where it came from, so it enters play exhausted.
playHeldAllyExhausted :: ReverseQueue m => IdentityId -> SupportId -> PlayerCard -> m ()
playHeldAllyExhausted you _ card = identityStep you (PlayingCard card Nothing)

attachAllyToMedLab :: ReverseQueue m => SupportId -> PlayerCard -> m ()
attachAllyToMedLab sid card = do
  removeFromGame card
  cardSpecific GameRef "medLabRemoveCapturedCard" (pcCardId card)
  cardSpecific (SupportRef sid) "holdAlly" card

-- | A card retrieved from underneath another card leaves that holder first.
-- Ordinary removeFromGame also serves captures from hand, deck and discard, whose
-- holder can already have recorded the card when the queued removal resolves.
removePlayerCardFromHeldZones :: (ReverseQueue m) => PlayerCard -> m ()
removePlayerCardFromHeldZones = push . RemovePlayerCardFromHeldZones . pcCardId


-- * NeXt Evolution

-- | Cancel a paid card before its effects or entry into play, retaining played
-- timing. Return whether there was still an effect to cancel.
cancelPlayerCardEffects :: ReverseQueue m => IdentityId -> PlayerCard -> m Bool
cancelPlayerCardEffects you card = do
  pending <- isQueued matches
  when pending do
    cancelMatchingMessage matches
    if cdCardType (getCardDef card) == EventType
      then cardSpecific GameRef "cancelledPlayedEvent" (you,card)
      else do
        removeFromGame (CardIdRef $ pcCardId card)
        discardCard card
  pure pending
 where
  matches = \case
    PutCardIntoPlay _ other _ _ True -> pcCardId other == pcCardId card
    _ -> False

-- | Cancel a played event's effect, retaining its payment and normal disposal.
cancelEventEffects :: HasQueue m => EventId -> m ()
cancelEventEffects eid = cancelMatchingMessage $ \case
  ResolveEventEffect _ (EventMessage event _) -> event == eid
  _ -> False

-- | The defeated scheme, already in its discard pile, enters the victory display.
addDefeatedSideSchemeToVictoryDisplay :: ReverseQueue m => SideSchemeId -> m ()
addDefeatedSideSchemeToVictoryDisplay = cardSpecific GameRef "cableDefeatedSchemeVictory"

-- | Put the actual side-scheme card back into play, retaining its identity and owner.
putVictorySideSchemeIntoPlay :: ReverseQueue m => IdentityId -> Card -> m ()
putVictorySideSchemeIntoPlay you card = cardSpecific GameRef "cableVictorySchemeIntoPlay" (you, card)

-- | Attach a legal upgrade from a discard pile, without playing or paying for it.
putUpgradeOnAllyFromDiscard :: ReverseQueue m => IdentityId -> AllyId -> PlayerCard -> m ()
putUpgradeOnAllyFromDiscard you aid card = cardSpecific GameRef "cableUpgradeFromDiscard" (you, aid, card)

-- | The obligation attaches to its player side scheme, leaving with that scheme.
attachObligationToSideScheme :: ReverseQueue m => ObligationId -> SideSchemeId -> m ()
attachObligationToSideScheme oid sid = cardSpecific (ObligationRef oid) "cableAttachSideScheme" sid

-- | Prevent damage to the enemy addressed by the imminent damage message.
preventEnemyDamage :: ReverseQueue m => EnemyId -> Natural -> m ()
preventEnemyDamage enemy n = do
  pending <- withQueue $ \queue -> (queue, headMay
    [(source, damage) | message <- concatMap queuedMessageLeaves queue,
      Just (target, source, damage) <- [enemyDamageMessage message], target == enemy])
  for_ pending $ \(source, damage) -> unless (damageUnpreventable damage) $ do
    incoming <- incomingEnemyDamage enemy source damage
    if damageAmount incoming <= n
      then do
        -- Prevention changes damage taken; a positive piercing attack still
        -- discards tough before the damage it would deal is prevented.
        when (damageAmount incoming > 0 && piercesTough damage) $
          removeAllTough $ case enemy of
            EnemyVillainId vid -> VillainCharacter vid
            EnemyMinionId mid -> MinionCharacter mid
            EnemyAttachmentId aid -> AttachmentCharacter aid
        cancelEnemyDamage enemy
      else replaceMatchingMessage prevent $ \case
        VillainMessage vid (VillainDamaged _ _) -> enemy == EnemyVillainId vid
        MinionMessage mid (MinionDamaged _ _) -> enemy == EnemyMinionId mid
        _ -> False
 where
  prevent = \case
    VillainMessage vid (VillainDamaged source damage) ->
      [VillainMessage vid (VillainDamaged source damage {damagePrevention = damagePrevention damage + n})]
    MinionMessage mid (MinionDamaged source damage) ->
      [MinionMessage mid (MinionDamaged source damage {damagePrevention = damagePrevention damage + n})]
    msg -> [msg]



-- | Prevent the entire imminent instance, including damage reduced or capped already.
cancelEnemyDamage :: HasQueue m => EnemyId -> m ()
cancelEnemyDamage enemy = cancelMatchingMessage $ \case
  VillainMessage vid (VillainDamaged _ _) -> enemy == EnemyVillainId vid
  MinionMessage mid (MinionDamaged _ _) -> enemy == EnemyMinionId mid
  _ -> False


shuffleEncounterDiscardIntoDeck :: ReverseQueue m => m ()
shuffleEncounterDiscardIntoDeck = push ShuffleEncounterDiscardIntoDeck

cancelVillainDefeat :: ReverseQueue m => VillainId -> m ()
cancelVillainDefeat vid = do
  Msg.cancelVillainDefeat vid
  cancelMatchingMessage $ \case
    VillainMessage other (VillainResolveDefeat _ _) -> vid == other
    VillainMessage other (VillainDefeatPending _) -> vid == other
    _ -> False

redirectEnemyAttackToAlly :: ReverseQueue m => EnemyId -> IdentityId -> AllyId -> m ()
redirectEnemyAttackToAlly enemy you aid = cardSpecific (enemyRef enemy) "neRedirectAttackToAlly" (you,aid)

attachDefeatedMinionToAlly :: ReverseQueue m => MinionId -> AllyId -> m ()
attachDefeatedMinionToAlly mid aid = cardSpecific GameRef "neAttachDefeatedMinion" (mid, aid)



-- | Replace this attack activation, preserving which player the scheme is against.
replaceVillainAttackWithScheme :: ReverseQueue m => VillainId -> m ()
replaceVillainAttackWithScheme vid = do
  you <- getBoostedPlayer (EnemyVillainId vid)
  cancelEnemyActivation vid
  villainStep vid VillainEndAttack
  villainSchemesAgainst vid you

-- | Discard every tough card, preserving the normal status-removal windows.
removeAllTough :: (ReverseQueue m, IsCharacter character) => character -> m ()
removeAllTough character = do
  n <- characterToughCount (toCharacter character)
  replicateM_ (fromIntegral n) $ removeStatus character ToughStatus

-- | Redirect the existing attack without creating a second activation.
redirectVillainAttackToCharacter :: ReverseQueue m => VillainId -> CharacterId -> m ()
redirectVillainAttackToCharacter vid character = cardSpecific GameRef "redirectVillainAttackToCharacter" (vid, character)

-- | Increase the imminent damage instance after its would-take-damage window opens.
increasePendingCharacterDamage :: (HasQueue m, IsCharacter c) => c -> Natural -> m ()
increasePendingCharacterDamage (toCharacter -> character) n = replaceMatchingMessage raise matchesDamage
 where
  matchesDamage = \case
    IdentityMessage you (IdentityDamaged _ _) -> character == IdentityCharacter you
    AllyMessage aid (AllyDamaged _ _) -> character == AllyCharacter aid
    _ -> False
  raise = \case
    IdentityMessage you (IdentityDamaged source damage) -> [IdentityMessage you (IdentityDamaged source damage {damageAmount = damageAmount damage + n})]
    AllyMessage aid (AllyDamaged source damage) -> [AllyMessage aid (AllyDamaged source damage {damageAmount = damageAmount damage + n})]
    msg -> [msg]

-- | A minion's attack with its own ATK change and gained keywords.
minionAttacksWithChange :: ReverseQueue m => MinionId -> IdentityId -> Int -> [Keyword] -> m ()
minionAttacksWithChange mid you n keywords = minionStep mid (MinionAttacksWithChange you n keywords)


-- | Flip one of Psylocke's two-sided weapons, retaining its identity and exhaustion.
flipPsiEnergy :: ReverseQueue m => UpgradeId -> m ()
flipPsiEnergy uid = cardSpecific uid "flipPsiEnergy" ()

putPsiEnergyIntoPlay :: ReverseQueue m => IdentityId -> m ()
putPsiEnergyIntoPlay you = do
  weapons <- filter (\c -> toCardCode c == "41002a" && pcOwner c == Just you) <$> getSetAsidePlayerCards
  for_ weapons $ \card -> do
    removeFromGame (CardIdRef $ pcCardId card)
    putIntoPlay you card


-- | Replace only the currently pending target of a player's attack. Subsequent
-- card-specific choices are left intact; they select their targets when reached.
redirectCurrentPlayerAttack :: ReverseQueue m => IdentityId -> EnemyId -> CharacterId -> m ()
redirectCurrentPlayerAttack you enemy friend = do
  current <- currentPlayerAttackSource you
  for_ current $ \source -> do
    pending <- withQueue $ \queue -> (queue, headMay [d | message <- concatMap queuedMessageLeaves queue, d <- damageFor source message])
    beforeTarget <- withQueue $ \queue -> (queue, headMay [d | CardSpecific GameRef "playerAttackDamageBeforeTarget" (fromJSON -> Success (who :: IdentityId, from :: Source, target :: EnemyId, d :: Damage)) <- concatMap queuedMessageLeaves queue, who == you, from == source, target == enemy])
    pendingRanged <- withQueue $ \queue -> (queue, headMay [ranged | EventMessage eid (EventAttacked who target ranged) <- concatMap queuedMessageLeaves queue, source == EventRef eid, who == you, target == enemy])
    withQueue_ $ concatMap (rewrite source beforeTarget)
    let inherited = beforeTarget <|> pending
        ranged = maybe (fromMaybe False pendingRanged) damageRanged inherited
        inheritedKeywords = maybe [] (\d -> [Overkill | damageOverkill d] <> [Piercing | damagePiercing d] <> [Ranged | damageRanged d]) inherited
        resolved = CardSpecific GameRef "psylockeFriendlyAttackResolved" (toJSON (you, source, friend, ranged))
    cardSpecific GameRef "psylockeRedirectAttackWindows" (you, source, enemy, friend, inheritedKeywords)
    case source of
      EventRef eid -> do
        cardSpecific (EventRef eid) "playerAttackStarted" ()
        replaceMatchingMessage (\msg -> [resolved, msg]) (== EventMessage eid ResolvedEvent)
      _ -> replaceMatchingMessage (\msg -> [resolved, msg]) (\case
        CheckWindows windows -> any (\w -> case windowType w of W.PlayerAttackResolved from attacker _ -> from == source && attacker == you; _ -> False) windows
        _ -> False)
 where
  damageFor source = \case
    VillainMessage vid (VillainDamaged from d) | enemy == EnemyVillainId vid && source == from && damageSource d == FromPlayerAttack you -> [d]
    MinionMessage mid (MinionDamaged from d) | enemy == EnemyMinionId mid && source == from && damageSource d == FromPlayerAttack you -> [d]
    _ -> []
  rewrite source beforeTarget message = case damageFor source message of
    d : _ -> [CardSpecific GameRef "psylockeRedirectedAttackDamage" (toJSON (you, source, friend, fromMaybe d beforeTarget))]
    [] -> case message of
      WithSimultaneousAttackContext cursor original -> map (WithSimultaneousAttackContext cursor) (rewrite source beforeTarget original)
      PrepareEnemyDamage original -> rewrite source beforeTarget original
      ResolveEnemyDamage original -> rewrite source beforeTarget original
      EventMessage eid (EventAttacked attacker target _) | source == EventRef eid && attacker == you && target == enemy -> []
      VillainMessage vid (VillainRetaliates (IdentityCharacter attacker)) | enemy == EnemyVillainId vid && attacker == you -> []
      MinionMessage mid (MinionRetaliates (IdentityCharacter attacker)) | enemy == EnemyMinionId mid && attacker == you -> []
      VillainMessage vid (VillainStunned from) | enemy == EnemyVillainId vid && from == source -> [GiveStatus (characterRef friend) StunnedStatus]
      MinionMessage mid (MinionStunned from) | enemy == EnemyMinionId mid && from == source -> [GiveStatus (characterRef friend) StunnedStatus]
      VillainMessage vid (VillainConfused from) | enemy == EnemyVillainId vid && from == source -> [GiveStatus (characterRef friend) ConfusedStatus]
      MinionMessage mid (MinionConfused from) | enemy == EnemyMinionId mid && from == source -> [GiveStatus (characterRef friend) ConfusedStatus]
      CheckWindows windows -> [CheckWindows filtered | let filtered = mapMaybe (keepWindow source beforeTarget) windows, notNull filtered]
      CheckWindowsFor players windows -> [CheckWindowsFor players filtered | let filtered = mapMaybe (keepWindow source beforeTarget) windows, notNull filtered]
      Simultaneously messages -> [Simultaneously $ concatMap (rewrite source beforeTarget) messages]
      _ -> [message]
  keepWindow source beforeTarget window = case windowType window of
    W.IdentityAttack attacker target | attacker == you && target == enemy ->
      if windowTiming window == When then Just window {windowType = W.FriendlyPlayerAttack you friend} else Nothing
    W.IdentityAttackTarget attacker target | attacker == you && target == enemy -> Nothing
    W.MadeBasicAttackAgainst attacker target | attacker == you && target == enemy -> Nothing
    W.MakingRangedAttack attacker from target | attacker == you && target == enemy && from == source -> Nothing
    W.MakingKeywordAttack attacker from target _ _ | attacker == you && target == enemy && from == source ->
      let keywords = maybe [] (\d -> [Overkill | damageOverkill d] <> [Piercing | damagePiercing d] <> [Ranged | damageRanged d]) beforeTarget
       in Just window {windowType = W.MakingFriendlyKeywordAttack you source friend keywords}
    W.PlayerAttackResolved from attacker targets | from == source && attacker == you ->
      Just window {windowType = W.PlayerAttackResolved from attacker (filter (/= enemy) targets)}
    _ -> Just window

-- | Saved resource-labelled positions for the metagame card's public board.
configureResourceSlots :: ReverseQueue m => UpgradeId -> [Resource] -> m ()
configureResourceSlots uid = cardSpecific uid "configureResourceSlots"

-- | Move one threat from a scheme to the chosen empty position. The game checks
-- removal restrictions and the position again before moving either component.
moveThreatToUpgradeSlot :: ReverseQueue m => UpgradeId -> SchemeId -> Int -> m ()
moveThreatToUpgradeSlot uid scheme slot = cardSpecific GameRef "moveThreatToUpgradeSlot" (uid, scheme, slot)

-- | Moving damage heals the source character (RR Move).
moveDamageToUpgradeSlot :: ReverseQueue m => UpgradeId -> CharacterId -> Int -> m ()
moveDamageToUpgradeSlot uid character slot = cardSpecific GameRef "moveDamageToUpgradeSlot" (uid, character, slot)

-- | Store a physical card facedown, with no permission to play it from there.
holdFacedownCard :: (ReverseQueue m, IsRef host) => host -> PlayerCard -> m ()
holdFacedownCard host card = do
  push $ RemovePlayerCardFromHeldZones (pcCardId card)
  removeFromGame card
  cardSpecific host "holdFacedownCard" card

releaseHeldCard :: (ReverseQueue m, IsRef host) => host -> IdentityId -> PlayerCard -> m ()
releaseHeldCard host you card = cardSpecific host "releaseHeldCard" card >> addToHand you card

-- | The actual pending placement, after earlier interrupts may have replaced it.
pendingThreatPlacement :: ReverseQueue m => SchemeId -> m (Maybe Natural)
pendingThreatPlacement scheme = withQueue \queue -> (queue, headMay $ mapMaybe amount queue)
 where
  amount = \case
    MainSchemeMessage sid (MainSchemePlaceThreat n) | scheme == SchemeMainSchemeId sid -> Just n
    SideSchemeMessage sid (SideSchemePlaceThreat n) | scheme == SchemeSideSchemeId sid -> Just n
    _ -> Nothing

-- | Replace the first still-pending placement with damage to the chosen character.
replaceThreatWithDamage :: (ReverseQueue m, IsRef source) => source -> SchemeId -> CharacterId -> m ()
replaceThreatWithDamage source scheme character = do
  pending <- pendingThreatPlacement scheme
  for_ pending $ \n -> when (n > 0) $
    insteadOf (Msg.isThreatPlacement scheme) $ damageCharacter character source (toDamage n FromAbility)

searchCollectionWeapon :: ReverseQueue m => IdentityId -> UpgradeId -> m ()
searchCollectionWeapon you host = cardSpecific GameRef "searchCollectionWeapon" (you, host)

swapHeldWeapon :: ReverseQueue m => IdentityId -> UpgradeId -> UpgradeId -> PlayerCard -> m ()
swapHeldWeapon you host outgoing incoming = cardSpecific GameRef "swapHeldWeapon" (you, host, outgoing, incoming)

-- | A card's lasting modifiers, independent of its source remaining in play.
lastingModifiers :: (ReverseQueue m, IsRef source) => source -> EntityMatcher -> [Modifier] -> Bool -> m ()
lastingModifiers source matcher modifiers roundEnd = cardSpecific GameRef "lastingModifiers" (toSource source, matcher, modifiers, roundEnd)

addToIdentityBasicPower :: (HasQueue m, HasGame m) => IdentityId -> Natural -> m ()
addToIdentityBasicPower = addToBasicPower

-- | Move a defeated minion out of play into an enemy's facedown boost cards.
giveDefeatedMinionAsBoost :: (ReverseQueue m, IsEnemy enemy) => MinionId -> enemy -> m ()
giveDefeatedMinionAsBoost mid enemy = cardSpecific GameRef "giveDefeatedMinionAsBoost" (mid, toEnemy enemy)

-- | Give this physical encounter card to an enemy as a facedown boost card.
giveCardAsBoost :: (ReverseQueue m, IsCard card, IsEnemy enemy) => card -> enemy -> m ()
giveCardAsBoost card enemy = cardSpecific GameRef "giveCardAsBoost" (toCard card, toEnemy enemy)

-- | One activation without a newly dealt boost card, including status replacements.
enemyActivatesWithoutBoost :: (ReverseQueue m, IsRef source, IsEnemy enemy) => source -> enemy -> IdentityId -> m ()
enemyActivatesWithoutBoost source enemy you = cardSpecific GameRef "enemyActivatesWithoutBoost" (toSource source, toEnemy enemy, you)

-- | Tucked encounter cards retain their physical identity and are out of play.
tuckedEncounterCards :: HasGame m => IdentityId -> m [EncounterCard]
tuckedEncounterCards = project PlayerIdentityTuckedEncounterCards

tuckEncounterCard :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
tuckEncounterCard you card = do
  cardSpecific GameRef "removeEncounterCardFromPiles" card
  checkWindows [Window Would $ W.WouldTuckEncounterCard you card]
  cardSpecific you "tuckEncounterCard" card

discardTuckedEncounterCard :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
discardTuckedEncounterCard you = cardSpecific you "discardTuckedEncounterCard"

tuckTopEncounterCard :: ReverseQueue m => IdentityId -> m ()
tuckTopEncounterCard you = getEncounterDeck >>= traverse_ (tuckEncounterCard you) . headMay

addToTargetedBasicThwart :: (HasQueue m) => IdentityId -> Natural -> m ()
addToTargetedBasicThwart you bonus = replaceMatchingMessage raise matches
 where
  matches (CardSpecific (IdentityRef who) "basicThwartAgainst" _) = who == you
  matches _ = False
  raise (CardSpecific target "basicThwartAgainst" (fromJSON -> Success (scheme :: SchemeId, general :: Natural, thwOnly :: Natural))) = [CardSpecific target "basicThwartAgainst" (toJSON (scheme, general, thwOnly + bonus))]
  raise msg = [msg]

-- | Replace a pending facedown encounter-card deal before drawing its card.
cancelDealtEncounterCard :: ReverseQueue m => IdentityId -> m ()
cancelDealtEncounterCard you = insteadOf matches (pure ())
 where
  matches = \case
    UsingDeck _ inner -> matches inner
    CardSpecific GameRef "fwActuallyDealEncounterCard" (fromJSON -> Success other) -> you == other
    CardSpecific GameRef "actuallyDealFoundEncounterCard" (fromJSON -> Success (other,_ :: EncounterCard)) -> you == other
    _ -> False

getIdentityDefenseForLastAttack :: HasGame m => IdentityId -> m Natural
getIdentityDefenseForLastAttack = project PlayerIdentityLastDefense

-- | Cancel only the scheme waiting after its would-scheme window.
cancelPendingEnemyScheme :: (ReverseQueue m, IsEnemy enemy) => enemy -> m ()
cancelPendingEnemyScheme (toEnemy -> enemy) = insteadOf matches (pure ())
 where
  matches (VillainMessage vid VillainBeginScheme) = enemy == EnemyVillainId vid
  matches (MinionMessage mid MinionBeginScheme) = enemy == EnemyMinionId mid
  matches _ = False

replacePendingAttackWithScheme :: (ReverseQueue m, IsEnemy enemy) => enemy -> IdentityId -> m ()
replacePendingAttackWithScheme (toEnemy -> enemy) you = do
  cancelEnemyActivation enemy
  cardSpecific (enemyRef enemy) "schemeReplacesAttack" True
  case enemy of
    EnemyVillainId vid -> villainSchemesAgainst vid you
    EnemyMinionId mid -> minionSchemesAgainst mid you
    EnemyAttachmentId _ -> pure ()

-- | Attach the linked physical card to a minion without paying its printed cost.
attachSetAsideUpgradeToMinion :: ReverseQueue m => IdentityId -> CardCode -> MinionId -> m ()
attachSetAsideUpgradeToMinion you code mid = do
  cards <- getSetAsidePlayerCards
  for_ (headMay [card | card <- cards, toCardCode card == code, pcOwner card `elem` [Nothing, Just you]]) $ \card -> do
    putIntoPlay you (card {pcOwner = Just you, pcController = Just you})
    upgradeStep (UpgradeId $ unCardId $ pcCardId card) (UpgradeAttachedToEnemy $ EnemyMinionId mid)

-- | Exchange one assigned facedown boost card with the encounter deck's top card.
swapFacedownBoostWithTop :: (ReverseQueue m, IsEnemy enemy) => enemy -> EncounterCard -> EncounterCard -> m ()
swapFacedownBoostWithTop enemy old replacement = do
  removeFromGame (CardIdRef $ ecCardId replacement)
  cardSpecific (enemyRef $ toEnemy enemy) "fwReplaceBoost" (old, replacement)
  putOnTopOfEncounterDeck [old]

-- | An encounter effect's discard is distinct from spending a tucked card by a player effect.
discardTuckedEncounterCardFromEncounter :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
discardTuckedEncounterCardFromEncounter you = cardSpecific you "discardTuckedEncounterCardFromEncounter"

-- | Tuck caused by an encounter effect, bypassing player-card tuck replacement interrupts.
tuckEncounterCardFromEncounter :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
tuckEncounterCardFromEncounter you card = do
  cardSpecific GameRef "removeEncounterCardFromPiles" card
  cardSpecific you "tuckEncounterCard" card

-- Append to Marvel.Message.Lifted under ally basic power helpers.
modifyAllyBasicAttack :: HasQueue m => AllyId -> (Damage -> Damage) -> m ()
modifyAllyBasicAttack aid change = replaceMatchingMessage apply matches
 where
  matches = \case
    AllyMessage who (AllyAttackLands _ _) -> who == aid
    AllyMessage who (AllyEvenAttackLands _ _) -> who == aid
    _ -> False
  apply = \case
    AllyMessage who (AllyAttackLands enemy damage) -> [AllyMessage who (AllyAttackLands enemy $ change damage)]
    AllyMessage who (AllyEvenAttackLands damage shares) -> [AllyMessage who (AllyEvenAttackLands (change damage) shares)]
    other -> [other]

-- | Redirect attachment plumbing before its pending attach resolves (Fixer).
redirectAttachmentTo :: HasQueue m => AttachmentId -> MinionId -> m ()
redirectAttachmentTo attachment minion = replaceMatchingMessage change matches
 where
  matches = \case
    AttachmentMessage aid (AttachedToEnemy _) -> aid == attachment
    VillainMessage _ (AttachedToVillain aid) -> aid == attachment
    _ -> False
  change = \case
    AttachmentMessage aid (AttachedToEnemy _) -> [AttachmentMessage aid (AttachedToEnemy $ EnemyMinionId minion)]
    VillainMessage _ (AttachedToVillain _) -> []
    msg -> [msg]

-- | Change the target of this pending attack, keeping its source, damage and
-- completion. A future attack selected by a card-specific step is untouched.
redirectPlayerAttackToEnemy :: ReverseQueue m => IdentityId -> Source -> EnemyId -> EnemyId -> m ()
redirectPlayerAttackToEnemy you source old new = withQueue_ $ map rewrite
 where
  damage from d | from == source = case new of
    EnemyAttachmentId aid -> AttachmentMessage aid $ AttachmentDamaged $ damageAmount d
    EnemyMinionId mid -> MinionMessage mid $ MinionDamaged from d
    EnemyVillainId vid -> VillainMessage vid $ VillainDamaged from d
  damage from d = case old of
    EnemyAttachmentId aid -> AttachmentMessage aid $ AttachmentDamaged $ damageAmount d
    EnemyMinionId mid -> MinionMessage mid $ MinionDamaged from d
    EnemyVillainId vid -> VillainMessage vid $ VillainDamaged from d
  retaliation c = case new of
    EnemyAttachmentId _ -> CheckWindows []
    EnemyMinionId mid -> MinionMessage mid $ MinionRetaliates c
    EnemyVillainId vid -> VillainMessage vid $ VillainRetaliates c
  rewrite = \case
    VillainMessage vid (VillainDamaged from d) | old == EnemyVillainId vid && from == source -> damage from d
    MinionMessage mid (MinionDamaged from d) | old == EnemyMinionId mid && from == source -> damage from d
    VillainMessage vid (VillainRetaliates c) | old == EnemyVillainId vid -> retaliation c
    MinionMessage mid (MinionRetaliates c) | old == EnemyMinionId mid -> retaliation c
    AllyMessage aid (AllyAttackLands enemy d) | source == AllyRef aid && enemy == old -> AllyMessage aid $ AllyAttackLands new d
    AllyMessage aid (AllyAttackConsequentialAgainst enemies) | source == AllyRef aid -> AllyMessage aid $ AllyAttackConsequentialAgainst (map target enemies)
    EventMessage eid (EventAttacked who enemy ranged) | source == EventRef eid && who == you && enemy == old -> EventMessage eid $ EventAttacked who new ranged
    CardSpecific GameRef "playerAttackDamageBeforeTarget" (fromJSON -> Success (who,from,enemy,d)) | who == you && from == source && enemy == old -> CardSpecific GameRef "playerAttackDamageBeforeTarget" $ toJSON (who,from,new,d :: Damage)
    PrepareEnemyDamage inner -> PrepareEnemyDamage $ rewrite inner
    ResolveEnemyDamage inner -> ResolveEnemyDamage $ rewrite inner
    WithSimultaneousAttackContext cursor inner -> WithSimultaneousAttackContext cursor $ rewrite inner
    Simultaneously messages -> Simultaneously $ map rewrite messages
    CheckWindows windows -> CheckWindows $ map window windows
    CheckWindowsFor players windows -> CheckWindowsFor players $ map window windows
    other -> other
  target enemy = if enemy == old then new else enemy
  window w = w {windowType = case windowType w of
    W.IdentityAttack who enemy | who == you -> W.IdentityAttack who $ target enemy
    W.IdentityAttackTarget who enemy | who == you -> W.IdentityAttackTarget who $ target enemy
    W.MadeBasicAttackAgainst who enemy | who == you -> W.MadeBasicAttackAgainst who $ target enemy
    W.AllyAttack aid enemy | source == AllyRef aid -> W.AllyAttack aid $ target enemy
    W.AllyAttackCompletedAgainst aid enemies | source == AllyRef aid -> W.AllyAttackCompletedAgainst aid $ map target enemies
    W.PlayerAttackResolved from who enemies | from == source && who == you -> W.PlayerAttackResolved from who $ map target enemies
    W.MakingRangedAttack who from enemy | from == source && who == you -> W.MakingRangedAttack who from $ target enemy
    W.MakingKeywordAttack who from enemy keywords basic | from == source && who == you -> W.MakingKeywordAttack who from (target enemy) keywords basic
    other -> other}

-- | A discarded encounter attachment enters play on a character without being revealed.
attachEncounterToCharacter :: ReverseQueue m => EncounterCard -> CharacterId -> m ()
attachEncounterToCharacter card character = cardSpecific GameRef "attachEncounterToCharacter" (card,character)

-- | The condition loses stalwart but replaces a stun or confuse before the
-- status can be given, so vulnerable does not also discard its host.
absorbStatusWithSolidSound :: ReverseQueue m => EnemyId -> m Bool
absorbStatusWithSolidSound enemy = do
  attachments <- selectList $ AttachmentOnEnemy enemy
  matching <- filterM (\aid -> maybe False ((== "50144") . toCardCode) <$> getsGame (`abilityCardDef` AttachmentRef aid)) attachments
  for_ (headMay matching) removeFromPlay
  pure $ notNull matching

-- | Move actual removable threat, with effects that change its destination respected.
moveSchemeThreatToSupport :: (ReverseQueue m, IsRef source) => source -> SchemeId -> SupportId -> Natural -> m ()
moveSchemeThreatToSupport source from sid n = do
  allowed <- canMoveSchemeThreatFrom source from
  when allowed $ do
    available <- selectCount Matchers.SchemeThreat (SchemeWithId from)
    let amount = min n available
    redirected <- elem RemovedThreatPlacedOnMinion <$> getModifiers (schemeRef from)
    when (amount > 0) $ do
      case from of
        SchemeMainSchemeId main -> mainSchemeStep main (MainSchemeThreatRemoved (toSource source) amount)
        SchemeSideSchemeId side -> sideSchemeStep side (SideSchemeThreatRemoved (toSource source) amount)
      unless redirected $ cardSpecific sid "movedThreatToSupport" amount

-- | Swap two physical encounter cards; this is neither a discard nor a tuck effect.
swapIdentityTuckedEncounterCard :: ReverseQueue m => IdentityId -> EncounterCard -> EncounterCard -> m ()
swapIdentityTuckedEncounterCard you incoming outgoing = do
  cardSpecific GameRef "removeEncounterCardFromPiles" incoming
  cardSpecific you "swapTuckedEncounterCard" (incoming,outgoing)

removeEncounterCardFromPiles :: ReverseQueue m => EncounterCard -> m ()
removeEncounterCardFromPiles = cardSpecific GameRef "removeEncounterCardFromPiles"

-- | Replace the pending discard from play, before its entity leaves.
preventCardDiscardFromPlay :: (ReverseQueue m, IsCard card) => card -> m ()
preventCardDiscardFromPlay card = cancelMatchingMessage $ \case
  DiscardedCard other -> toCardId other == toCardId (toCard card)
  RemoveFromPlayAfterWindow target -> matches target
  ResolveAllyLeavingPlay _ aid _ _ -> matches (AllyRef aid)
  CardSpecific GameRef "afterDiscardFromPlay" (fromJSON -> Success other) -> toCardId (other :: Card) == toCardId (toCard card)
  _ -> False
 where
  matches target = case target of
    AllyRef aid -> unAllyId aid == unCardId (toCardId (toCard card))
    SupportRef sid -> unSupportId sid == unCardId (toCardId (toCard card))
    UpgradeRef uid -> unUpgradeId uid == unCardId (toCardId (toCard card))
    SideSchemeRef sid -> unSideSchemeId sid == unCardId (toCardId (toCard card))
    _ -> False

-- | Skip the entire next villain phase, including its timing windows.
attachUpgradeToTarget :: (ReverseQueue m, IsRef target) => UpgradeId -> target -> m ()
attachUpgradeToTarget uid target = upgradeStep uid (UpgradeAttachedToTarget $ toTarget target)

skipNextVillainPhase :: ReverseQueue m => m ()
skipNextVillainPhase = cardSpecific GameRef "skipNextVillainPhase" ()

untilNextVillainPhase :: (ReverseQueue m, IsRef source, IsRef target) => source -> target -> [Modifier] -> m ()
untilNextVillainPhase source target modifiers = cardSpecific GameRef "untilNextVillainPhase" (toSource source, toTarget target, modifiers)

playHeldCardAsHand :: (ReverseQueue m, IsRef host) => IdentityId -> host -> PlayerCard -> m ()
playHeldCardAsHand you host card = releaseHeldCard host you card >> playCardWithReduction you 0 card

discardHeldPlayerCard :: (ReverseQueue m, IsRef host) => IdentityId -> host -> PlayerCard -> m ()
discardHeldPlayerCard _ host card = cardSpecific host "releaseHeldCard" card >> discardCard card

-- Resolve only the discarded treachery's printed When Revealed ability.
resolveEncounterWhenRevealed :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
resolveEncounterWhenRevealed you card = cardSpecific GameRef "resolveEncounterWhenRevealed" (you,card)

dealMinionAsEncounterCard :: ReverseQueue m => MinionId -> IdentityId -> m ()
dealMinionAsEncounterCard mid you = cardSpecific GameRef "dealMinionAsEncounterCard" (mid,you)

revealEncounterCardWithPlayerCount :: ReverseQueue m => IdentityId -> Natural -> EncounterCard -> m ()
revealEncounterCardWithPlayerCount you count card = cardSpecific GameRef "revealEncounterCardWithPlayerCount" (you,fromIntegral count :: Int,card)

preventCharacterDamage :: (ReverseQueue m, IsCharacter character) => character -> Natural -> m ()
preventCharacterDamage character n = case toCharacter character of
  IdentityCharacter you -> Msg.preventDamage you n
  AllyCharacter aid -> Msg.preventAllyDamage aid n
  VillainCharacter vid -> preventEnemyDamage (EnemyVillainId vid) n
  MinionCharacter mid -> preventEnemyDamage (EnemyMinionId mid) n
  AttachmentCharacter aid -> preventEnemyDamage (EnemyAttachmentId aid) n

basicThwartIgnoringPatrolAndCrisis :: ReverseQueue m => IdentityId -> Natural -> m ()
basicThwartIgnoringPatrolAndCrisis you bonus = cardSpecific GameRef "basicThwartIgnoringPatrolAndCrisis" (you,bonus)

-- Tuck is faceup and grants no permission to play the card (RR Tuck).
tuckPlayerCardUnder :: (ReverseQueue m, IsRef host) => host -> PlayerCard -> m ()
tuckPlayerCardUnder host card = removeFromGame card >> cardSpecific host "holdTuckedCard" card

-- Preserve the actual pending amount after threat interrupts, before the board can
-- advance or a response can remove threat. Nested schemes retain their own receipt.
placeVillainSchemeThreat :: ReverseQueue m => VillainId -> SchemeId -> Natural -> m ()
placeVillainSchemeThreat vid scheme n = cardSpecific GameRef "placeVillainThreatBy" (VillainRef vid,ThreatFromVillain,scheme,n)

placeVillainSchemeThreatAfterCharacterWindow :: ReverseQueue m => VillainId -> SchemeId -> Natural -> m ()
placeVillainSchemeThreatAfterCharacterWindow vid scheme n = do
  when (n > 0) $ checkWindows [Window Would $ W.ThreatPlaced ThreatFromVillain scheme n]
  cardSpecific (VillainRef vid) "recordPendingSchemeThreat" scheme
  push $ threatPlacement scheme n

recordPendingVillainSchemeThreat :: ReverseQueue m => VillainId -> SchemeId -> m ()
recordPendingVillainSchemeThreat vid scheme = do
  amount <- fromMaybe 0 <$> pendingThreatPlacement scheme
  present <- selectAny (SchemeWithId scheme)
  let receipt = if present then amount else 0
  let update = \case
        CardSpecific _ "villainSchemeResultOn" (fromJSON -> Success (previous,total :: Natural)) -> [Msg.cardSpecific (VillainRef vid) "villainSchemeResultOn" (previous :: SchemeId,total+receipt)]
        _ -> [Msg.cardSpecific (VillainRef vid) "villainSchemeResultOn" (scheme,receipt)]
  replaceMatchingMessage update $ \case
    CardSpecific (VillainRef who) key _ -> who == vid && key `elem` ["villainSchemeResult","villainSchemeResultOn"]
    _ -> False

-- | Resolve only the boost text of a minion that is already in play. No boost
-- icons, card flip, entry keywords or boost discard are implied.
resolveInPlayMinionBoost :: ReverseQueue m => IdentityId -> MinionId -> Maybe EnemyId -> m ()
resolveInPlayMinionBoost you mid actor = cardSpecific GameRef "beginInPlayMinionBoost" (mid,you,actor)


-- | Flip across card types, discarding the old face's attachments and counters.
flipSideSchemeToObligation :: ReverseQueue m => SideSchemeId -> IdentityId -> CardCode -> m ()
flipSideSchemeToObligation sid you code = cardSpecific GameRef "flipSideSchemeToObligation" (sid,you,code)

-- | Replace taking damage with placing the full packet on an attachment. This is
-- a replacement, so damage that cannot be prevented can still change destination.
placeCharacterDamageOnAttachment :: (ReverseQueue m, IsCharacter character) => character -> AttachmentId -> Damage -> m ()
placeCharacterDamageOnAttachment (toCharacter -> character) aid damage = insteadOf matches do
  when (piercesTough damage) $ removeAllTough character
  damageAttachment aid (damageAmount damage)
 where
  matches = \case
    IdentityMessage you (IdentityDamaged _ _) -> character == IdentityCharacter you
    AllyMessage ally (AllyDamaged _ _) -> character == AllyCharacter ally
    VillainMessage villain (VillainDamaged _ _) -> character == VillainCharacter villain
    MinionMessage minion (MinionDamaged _ _) -> character == MinionCharacter minion
    _ -> False

-- | Replace the destination without re-opening the replaced character window.
redirectCharacterThreatPlacement :: ReverseQueue m => SchemeId -> SideSchemeId -> m ()
redirectCharacterThreatPlacement old new = replaceMatchingMessage rewrite matches
 where
  matches = \case
    CardSpecific GameRef key (fromJSON -> Success (_ :: Source,_ :: ThreatSource,scheme :: SchemeId,_ :: Natural)) ->
      key `elem` ["placeThreatAfterCharacterWindow","placeReportingThreatAfterCharacterWindow","placeVillainThreatAfterCharacterWindow"] && scheme == old
    _ -> False
  rewrite = \case
    CardSpecific target key (fromJSON -> Success (source :: Source,from :: ThreatSource,_ :: SchemeId,n :: Natural)) ->
      [Msg.cardSpecific target key (source,from,SchemeSideSchemeId new,n)]
    message -> [message]

redirectCharacterThreatRemoval :: ReverseQueue m => SchemeId -> SideSchemeId -> Natural -> m ()
redirectCharacterThreatRemoval old new n = replaceMatchingMessage rewrite matches
 where
  matches message = maybe False (\(_,scheme,_,_,_) -> scheme == old) (Msg.threatRemovalMessage message)
  rewrite message = case Msg.threatRemovalMessage message of
    Just (source,_,_,_,thwarting) -> [SideSchemeMessage new $ if thwarting then SideSchemeThwarted source n else SideSchemeThreatRemoved source n]
    Nothing -> [message]

-- | Finish a treachery already moved to a facedown boost, without discarding it
-- or marking it removed from the game. Its resolution response still opens.
resolveTreacheryAsBoost :: ReverseQueue m => TreacheryId -> IdentityId -> EncounterCard -> m ()
resolveTreacheryAsBoost tid you card = cardSpecific GameRef "resolveTreacheryAsBoost" (tid,you,card)

-- | Modifiers created by an activation interrupt persist through its resolution,
-- independently of the source remaining in play or becoming blank.
untilEnemyActivationEnds :: (ReverseQueue m, IsRef source, IsEnemy enemy) => source -> enemy -> [Modifier] -> m ()
untilEnemyActivationEnds source enemy modifiers = cardSpecific GameRef "untilEnemyActivationEnds" (toSource source,toEnemy enemy,modifiers)

-- | Defeat still occurs and keeps its response windows, but its departure is to
-- the removed-from-game area instead of the discard pile or victory display.
removeDefeatedAllyFromGame :: ReverseQueue m => AllyId -> m ()
removeDefeatedAllyFromGame aid = replaceMatchingMessage replace matches
 where
  matches = \case PrepareAllyDepartureByDefeat who _ -> who == aid; _ -> False
  replace = \case
    PrepareAllyDepartureByDefeat who _ -> [PrepareAllyDepartureByDefeat who $ RemoveFromGame $ AllyRef who]
    message -> [message]

finishRaiseTheStakes :: ReverseQueue m => EncounterCard -> Bool -> m ()
finishRaiseTheStakes card defeated = cardSpecific GameRef "finishRaiseTheStakes" (card,defeated)

-- | A constant loss can also affect an attack already in progress.
attackLoses :: (ReverseQueue m, IsEnemy enemy) => enemy -> Keyword -> m ()
attackLoses enemy keyword = cardSpecific GameRef "enemyAttackLosesKeyword" (toEnemy enemy,keyword)

preventUpgradeReady :: ReverseQueue m => UpgradeId -> m ()
preventUpgradeReady uid = cancelMatchingMessage (== UpgradeMessage uid ReadiedUpgrade)

-- | The ready has already passed its interrupt window and payment.
readyUpgradeAfterInterrupt :: ReverseQueue m => UpgradeId -> m ()
readyUpgradeAfterInterrupt = push . ResolveReadiedUpgrade

-- | Discard through the matching card, then reveal that physical discarded card.
-- This continuation survives the source of the instruction leaving play.
discardUntilAndReveal :: ReverseQueue m => IdentityId -> CardMatcher -> m ()
discardUntilAndReveal you matcher = push $ DiscardUntilAndReveal you matcher

-- | Replace this particular facedown encounter card after looking at it.
replaceDealtEncounterCard :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
replaceDealtEncounterCard you card = cardSpecific GameRef "replaceDealtEncounterCard" (you,card)

-- | Discard a particular card seen in the encounter deck, preserving discard responses.
discardKnownEncounterCardFromDeck :: ReverseQueue m => IdentityId -> EncounterCard -> m ()
discardKnownEncounterCardFromDeck you card = do
  cards <- getEncounterDeck
  when (card `elem` cards) $ do
    removeEncounterCardFromPiles card
    discardCard card
    checkWindows [W.Window W.After $ W.PlayerDiscardedFromEncounterDeck you card]

preventBoostAssignment :: (HasQueue m,IsEnemy enemy) => enemy -> m ()
preventBoostAssignment enemy = cancelMatchingMessage $ (== Just (toEnemy enemy)) . boostAssignmentEnemy
