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

import Marvel.Ability.Types (Ability)
import Marvel.ActiveCost.Types
import Marvel.Attack (Attack (..))
import Marvel.Card.Code (CardCode)
import Marvel.Card.Def (CardDef (..), CardMatcher, HasCardDef (..))
import Marvel.Card.EncounterCard (EncounterCard)
import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Card.Side (Side)
import Marvel.Card.Types (Card, IsCard (..))
import Marvel.Cost.Types (Cost)
import Marvel.Choice qualified as Choice
import Marvel.Damage
import Marvel.Exception (NoChoices (..))
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Message as X hiding (cardSpecific, damageIdentity, engageMinion, enemyRetaliates)
import Marvel.Message qualified as Msg
import Marvel.Message.Lifted.Queue as X
import Marvel.Phase (Phase)
import Marvel.Question
import Marvel.Queue
import Marvel.Query (selectJust)
import Marvel.Keyword (Keyword (Incite))
import Marvel.Queue as X (cancelMatchingMessage, replaceMatchingMessage)
import Marvel.Ref
import Marvel.Window.Types (ThreatSource (..), Window (..), WindowTiming (After, When, Would))
import Marvel.Window.Types qualified as W

-- * Questions
--
-- A question is its choices, each built from what choosing it does: @chooseOne you
-- [targeting enemy (dealDamage enemy attrs damage) | enemy <- enemies]@.

chooseOne :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOne you choices = push . Ask you . ChooseOne =<< sequence choices

-- | Ask one of the choices, saying what is being asked.
chooseOnePrompted :: (ReverseQueue m) => IdentityId -> Text -> [m Choice] -> m ()
chooseOnePrompted you prompt choices = push . Ask you . prompted prompt . ChooseOne =<< sequence choices

-- | Ask one of the choices, saying what is being asked about which card.
chooseOnePromptedAbout :: (ReverseQueue m, IsRef a) => IdentityId -> Text -> a -> [m Choice] -> m ()
chooseOnePromptedAbout you prompt a choices = push . Ask you . promptedAbout prompt a . ChooseOne =<< sequence choices

chooseOneAtATime :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOneAtATime you choices = push . Ask you . ChooseOneAtATime =<< sequence choices

-- | Ask each of the choices in the order the player picks, saying what is being asked.
chooseOneAtATimePrompted :: (ReverseQueue m) => IdentityId -> Text -> [m Choice] -> m ()
chooseOneAtATimePrompted you prompt choices = push . Ask you . prompted prompt . ChooseOneAtATime =<< sequence choices

-- | Ask, unless there is only the one choice: that is made. None is an error.
chooseOrRunOne :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrRunOne you choices =
  sequence choices >>= \case
    [] -> throwM NoChoices
    [choice] -> resolveAs you choice
    choices' -> push $ Ask you $ ChooseOne choices'

-- | A choice offered on the game element, doing what the body does.
targeting :: (ReverseQueue m, IsRef a) => a -> m () -> m Choice
targeting a body = TargetLabel (toTarget a) . pure . Run <$> capture body

-- | A choice by its label (an i18n key under @$label.@), doing what the body does.
labeled :: (ReverseQueue m) => Text -> m () -> m Choice
labeled l body = Label l . pure . Run <$> capture body

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

-- | Damage dealt to each of the enemies as one effect ('simultaneousDamage').
dealDamageToEach :: (ReverseQueue m, IsEnemy enemy, IsRef source) => [enemy] -> source -> Damage -> m ()
dealDamageToEach enemies source damage =
  pushAll . simultaneousDamage =<< traverse (\enemy -> capture $ dealDamage enemy source damage) enemies

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
dealIndirectDamage ident source n = indirectDamageAssigned ident (toSource source) n []

-- | The indirect damage left to assign, and what has been.
indirectDamageAssigned :: (ReverseQueue m) => IdentityId -> Source -> Natural -> [(CharacterId, Natural)] -> m ()
indirectDamageAssigned ident source n = push . AssignIndirectDamage ident source n

setHitPoints :: (ReverseQueue m) => IdentityId -> Natural -> m ()
setHitPoints ident = push . IdentityMessage ident . IdentitySetHitPoints

-- * Threat

thwartScheme :: (ReverseQueue m, IsScheme scheme, IsRef source) => scheme -> source -> Natural -> m ()
thwartScheme scheme source = resolve . ThwartScheme (SchemeRef $ toScheme scheme) (toSource source)

-- | The threat placed on the scheme without the "would place threat" window
-- ('threatPlacement'): what an interrupt to that window places in its stead.
placeThreatWithoutWindow :: (ReverseQueue m, IsScheme scheme) => scheme -> Natural -> m ()
placeThreatWithoutWindow scheme = push . threatPlacement (toScheme scheme)

-- | Threat removed from a scheme matching, the player choosing which.
removeThreat :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> SchemeMatcher -> m ()
removeThreat you source n = resolveAs you . RemoveThreat (toSource source) n

-- | Threat a card places on the scheme, the "would place threat" window first
-- ('placeThreatOn').
placeThreat :: (ReverseQueue m, IsScheme scheme) => scheme -> Natural -> m ()
placeThreat = placeThreatFrom ThreatFromAbility

-- | Threat placed on the scheme by what the window says, the window first.
placeThreatFrom :: (ReverseQueue m, IsScheme scheme) => ThreatSource -> scheme -> Natural -> m ()
placeThreatFrom from (toScheme -> scheme) n =
  pushAll $ [CheckWindows [Window Would $ W.ThreatPlaced from scheme n] | n > 0] <> [threatPlacement scheme n]

-- | Threat placed on each scheme matching.
placeThreatOnEach :: (ReverseQueue m, IsRef source) => source -> Natural -> SchemeMatcher -> m ()
placeThreatOnEach source n = resolve . PlaceThreat (toSource source) n

-- | Incite X, as the card is revealed: X threat on the main scheme.
incite :: (ReverseQueue m, HasCardDef card) => card -> m ()
incite card = for_ [n | Incite n <- toList $ cdKeywords (getCardDef card)] \n -> do
  scheme <- selectJust MainScheme
  placeThreat scheme n

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

-- | Counters placed on, or removed from, an environment, main scheme or attachment.
placeCounters :: (ReverseQueue m, IsRef a) => a -> Natural -> m ()
placeCounters a = push . PlaceCounters (toTarget a)

removeCounters :: (ReverseQueue m, IsRef a) => a -> Natural -> m ()
removeCounters a = push . RemoveCounters (toTarget a)

-- * Players and allies

-- | More DEF for the hero's defense under way.
addDefense :: (ReverseQueue m) => IdentityId -> Natural -> m ()
addDefense ident = push . IdentityMessage ident . IdentityDefended

-- | What can be readied.
class Readyable a where
  readied :: a -> Message

instance Readyable IdentityId where
  readied ident = IdentityMessage ident ReadiedIdentity

instance Readyable AllyId where
  readied ally = AllyMessage ally ReadiedAlly

instance Readyable SupportId where
  readied support = SupportMessage support ReadiedSupport

instance Readyable UpgradeId where
  readied upgrade = UpgradeMessage upgrade ReadiedUpgrade

ready :: (ReverseQueue m, Readyable a) => a -> m ()
ready = push . readied

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

defeatAlly :: (ReverseQueue m) => AllyId -> m ()
defeatAlly ally = push $ AllyMessage ally AllyDefeated

-- | The hero's basic powers, ATK or THW given the bonus, and the defense applied.
basicAttack, basicThwart :: (ReverseQueue m) => IdentityId -> Natural -> m ()
basicAttack ident = push . IdentityMessage ident . SideMessage . BasicAttack
basicThwart ident = push . IdentityMessage ident . SideMessage . BasicThwart

defenseApplied :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> m ()
defenseApplied ident = push . IdentityMessage ident . SideMessage . DefenseApplied . toEnemy

-- | The identity is defeated, past the window for it ("would be defeated").
defeatIdentity :: (ReverseQueue m) => IdentityId -> m ()
defeatIdentity ident =
  pushAll [CheckWindows [Window Would $ W.IdentityDefeat ident], IdentityMessage ident IdentityDefeated]

-- | The player's turn is over: what lasts for the turn ends.
identityEndedTurn :: (ReverseQueue m) => IdentityId -> m ()
identityEndedTurn = push . IdentityEndedTurn

-- | The identity flips to its other form.
changeForm :: (ReverseQueue m) => IdentityId -> m ()
changeForm ident = push $ IdentityMessage ident ChooseOtherForm

changeToForm :: (ReverseQueue m) => IdentityId -> Side -> m ()
changeToForm ident = push . IdentityMessage ident . ChangedToForm

-- * Attacks and defense

enemyAttacksAlly :: (ReverseQueue m, IsEnemy enemy) => enemy -> IdentityId -> AllyId -> m ()
enemyAttacksAlly enemy ident ally = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAttacksAlly ident ally)
  EnemyMinionId mid -> MinionMessage mid (MinionAttacksAlly ident ally)

-- | The enemy's attack is defended by the character.
enemyDefendedBy :: (ReverseQueue m, IsEnemy enemy) => enemy -> CharacterId -> m ()
enemyDefendedBy enemy character = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainDefendedBy character)
  EnemyMinionId mid -> MinionMessage mid (MinionDefendedBy character)

enemyAttacks :: (ReverseQueue m, IsEnemy enemy) => enemy -> IdentityId -> m ()
enemyAttacks enemy ident = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAttacks ident)
  EnemyMinionId mid -> MinionMessage mid (MinionAttacks ident)

-- | The enemy's retaliate against the character that attacked it.
enemyRetaliates :: (ReverseQueue m, IsEnemy enemy) => enemy -> CharacterId -> m ()
enemyRetaliates enemy = push . Msg.enemyRetaliates (toEnemy enemy)

-- | The character the attack is against is told it was attacked.
wasAttacked :: (ReverseQueue m) => Attack -> m ()
wasAttacked a = push case attackCharacter a of
  IdentityCharacter ident -> IdentityMessage ident (IdentityWasAttacked a)
  AllyCharacter ally -> AllyMessage ally (AllyWasAttacked a)
  _ -> error "Invalid damage target"

declareDefense :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> DefensePriority -> m ()
declareDefense ident enemy = push . DeclareDefense ident (toEnemy enemy)

-- | Defenders are asked for again, the players who have declined not asked.
declareDefenseWithout :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> DefensePriority -> [IdentityId] -> m ()
declareDefenseWithout ident enemy priority = push . DeclareDefenseWithout ident (toEnemy enemy) priority

-- * Villains

villainSchemes :: (ReverseQueue m) => VillainId -> m ()
villainSchemes vid = push $ VillainMessage vid VillainSchemes

advanceVillain :: (ReverseQueue m) => VillainId -> m ()
advanceVillain vid = villainStep vid VillainAdvanced

defeatVillain :: (ReverseQueue m) => VillainId -> m ()
defeatVillain vid = villainStep vid VillainDefeated

changeVillainHitPoints :: (ReverseQueue m) => VillainId -> Int -> m ()
changeVillainHitPoints vid = villainStep vid . VillainHitPointsChanged

-- | A boost card dealt facedown to the enemy, villain or minion: "give this enemy another
-- boost card" as its boost cards are turned up.
dealBoost :: (ReverseQueue m, IsRef enemy) => enemy -> m ()
dealBoost = push . DealBoost . toTarget

revealBoostCard :: (ReverseQueue m, IsEnemy enemy) => EncounterCard -> enemy -> m ()
revealBoostCard card = push . RevealBoostCard card . toEnemy

clearBoosts :: (ReverseQueue m) => m ()
clearBoosts = push ClearBoosts

-- | The boost card stays in play, under the player.
putBoostIntoPlay :: (ReverseQueue m, IsRef card) => card -> IdentityId -> m ()
putBoostIntoPlay card = push . PutBoostIntoPlay (toTarget card)

-- | More boost icons for the villain's activation.
addBoost :: (ReverseQueue m) => VillainId -> Natural -> m ()
addBoost vid = push . VillainMessage vid . VillainAddBoost

-- | "The attack gains overkill" (piercing, ranged): the enemy's attack under way.
attackGains :: (ReverseQueue m, IsEnemy enemy) => enemy -> Keyword -> m ()
attackGains enemy k = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (VillainAttackGains k)
  EnemyMinionId mid -> MinionMessage mid (MinionAttackGains k)

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

healAllDamage :: (ReverseQueue m) => MinionId -> m ()
healAllDamage mid = push $ MinionMessage mid MinionHealAllDamage

-- | The minion engages the player, leaving the player it was engaged with.
engageMinion :: (ReverseQueue m) => MinionId -> IdentityId -> m ()
engageMinion minion = pushAll . Msg.engageMinion minion

defeatMinion :: (ReverseQueue m) => MinionId -> m ()
defeatMinion mid = push $ MinionMessage mid MinionDefeated

-- | The player is no longer engaged with the minion.
minionDisengages :: (ReverseQueue m) => IdentityId -> MinionId -> m ()
minionDisengages ident = push . IdentityMessage ident . MinionDisengaged

-- | The minion's attack against the player, past its would window.
minionBeginsAttack :: (ReverseQueue m) => MinionId -> IdentityId -> m ()
minionBeginsAttack mid = push . MinionMessage mid . MinionBeginAttack

minionAttacked :: (ReverseQueue m) => MinionId -> m ()
minionAttacked mid = push $ MinionMessage mid MinionAttacked

minionEndsAttack :: (ReverseQueue m) => MinionId -> m ()
minionEndsAttack mid = push $ MinionMessage mid MinionEndAttack

-- | The minion's scheme, it not confused: threat on the main scheme.
minionSchemed :: (ReverseQueue m) => MinionId -> m ()
minionSchemed mid = push $ MinionMessage mid MinionSchemed

-- * Attachments and environments

-- | Whatever has the attachment is told it has left play.
attachmentRemoved :: (ReverseQueue m) => AttachmentId -> m ()
attachmentRemoved = push . AttachmentRemoved

-- | The attachment is attached to the enemy ('AttachedToEnemy').
attachTo :: (ReverseQueue m, IsEnemy enemy) => AttachmentId -> enemy -> m ()
attachTo attachment = push . AttachmentMessage attachment . AttachedToEnemy . toEnemy

-- | The enemy is told the attachment is attached to it.
attachedTo :: (ReverseQueue m, IsEnemy enemy) => enemy -> AttachmentId -> m ()
attachedTo enemy attachment = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (AttachedToVillain attachment)
  EnemyMinionId mid -> MinionMessage mid (AttachedToMinion attachment)

-- | The attachment is attached to the player's identity card.
attachToIdentity :: (ReverseQueue m) => AttachmentId -> IdentityId -> m ()
attachToIdentity attachment = push . AttachmentMessage attachment . AttachedToIdentity

attachToSideScheme :: (ReverseQueue m) => AttachmentId -> SideSchemeId -> m ()
attachToSideScheme attachment = push . AttachmentMessage attachment . AttachedToSideScheme

attachToEnvironment :: (ReverseQueue m) => EnvironmentId -> AttachmentId -> m ()
attachToEnvironment environment = push . EnvironmentMessage environment . AttachedToEnvironment

damageAttachment :: (ReverseQueue m) => AttachmentId -> Natural -> m ()
damageAttachment attachment = push . AttachmentMessage attachment . AttachmentDamaged

-- | The environment turns to its other face.
flipEnvironment :: (ReverseQueue m) => EnvironmentId -> m ()
flipEnvironment eid = push $ EnvironmentMessage eid FlipEnvironment

-- * Schemes

advanceMainScheme :: (ReverseQueue m) => m ()
advanceMainScheme = push AdvanceMainScheme

nextMainScheme :: (ReverseQueue m) => m ()
nextMainScheme = push NextMainScheme

-- | The side scheme is defeated, past the window for it, and leaves play.
defeatSideScheme :: (ReverseQueue m) => SideSchemeId -> m ()
defeatSideScheme sid =
  pushAll [CheckWindows [Window When $ W.DefeatedSideScheme sid], SideSchemeMessage sid DefeatSideScheme, RemoveFromPlay (toTarget sid)]

-- | What the side scheme does once what it revealed is done.
sideSchemeRevealedThen :: (ReverseQueue m) => SideSchemeId -> m ()
sideSchemeRevealedThen sid = push $ SideSchemeMessage sid SideSchemeRevealedThen

searchForAndRevealScheme :: (ReverseQueue m) => CardDef -> m ()
searchForAndRevealScheme = push . SearchForAndRevealScheme

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

placeOnBottomOfDeck :: (ReverseQueue m) => IdentityId -> PlayerCard -> m ()
placeOnBottomOfDeck ident = push . IdentityMessage ident . PlaceOnBottomOfDeck

shuffleDeck :: (ReverseQueue m) => IdentityId -> m ()
shuffleDeck ident = push $ IdentityMessage ident ShuffleDeck

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
putIntoPlay ident card = push $ PutCardIntoPlay ident card NoPayment Nothing

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
  VillainCharacter vid -> VillainMessage vid (AttachedUpgradeToVillain u)
  IdentityCharacter _ -> error "an upgrade on an identity is controlled, not attached"

-- | Whatever has the upgrade attached is told it has left play.
upgradeRemoved :: (ReverseQueue m) => UpgradeId -> m ()
upgradeRemoved = push . UpgradeRemoved

-- * Paying

-- | The player pays the cost for an encounter card, as a treachery asks.
payForTreachery :: (ReverseQueue m) => IdentityId -> Cost -> m ()
payForTreachery ident cost = getRandom >>= \cid -> setActiveCost $ ActiveCost cid ident ForTreachery cost NoPayment Nothing mempty

-- | The cost is begun on.
createdActiveCost :: (ReverseQueue m) => ActiveCostId -> m ()
createdActiveCost = push . CreatedActiveCost

-- | The cost is to be paid, as it says.
setActiveCost :: (ReverseQueue m) => ActiveCost -> m ()
setActiveCost = push . SetActiveCost

-- | The card is spent on the cost.
spendCard :: (ReverseQueue m) => ActiveCostId -> PlayerCard -> m ()
spendCard acid = push . Spent acid

-- | Part of the cost paid.
paid :: (ReverseQueue m) => Payment -> m ()
paid = push . Paid

-- | The part of the cost is paid ('PayCost').
payCost :: (ReverseQueue m) => ActiveCostId -> Cost -> m ()
payCost acid = push . PayCost acid

-- | The resources still to pay for the cost are asked for.
checkPayment :: (ReverseQueue m) => ActiveCostId -> m ()
checkPayment = push . CheckPayment

-- | The resources are paid: the rest of the cost follows.
finishPayment :: (ReverseQueue m) => ActiveCostId -> m ()
finishPayment = push . FinishedPayment

-- | The cost cannot be paid: what was spent on it goes back.
abortPayment :: (ReverseQueue m) => ActiveCostId -> m ()
abortPayment = push . AbortPayment

-- | The whole cost is paid: what it was paid for follows.
paidCosts :: (ReverseQueue m) => ActiveCostId -> m ()
paidCosts = push . PaidCosts

disableActiveCost :: (ReverseQueue m) => ActiveCostId -> m ()
disableActiveCost = push . DisableActiveCost

-- | The card is played, paid for: it enters play and the window after it opens.
cardPlayed :: (ReverseQueue m) => IdentityId -> PlayerCard -> Payment -> Maybe Window -> m ()
cardPlayed ident card payment mWindow =
  pushAll
    [ IdentityMessage ident (PlayedCard card mWindow)
    , PutCardIntoPlay ident card payment mWindow
    , CheckWindows [Window After $ W.CardPlayed ident card]
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
revealEncounterCardWithoutWindows ident = push . RevealedEncounterCard ident

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

-- | The player reveals the encounter card, its "when revealed" windows answered.
revealEncounterCard :: (ReverseQueue m) => IdentityId -> EncounterCard -> m ()
revealEncounterCard ident = push . RevealEncounterCard ident

-- | The top cards of the encounter deck are discarded, the target told which
-- ('WithDiscarded').
discardTopOfEncounterDeckFor :: (ReverseQueue m, IsRef target) => Natural -> target -> m ()
discardTopOfEncounterDeckFor n = push . DiscardTopOfEncounterDeck n . Just . toTarget

shuffleIntoEncounterDeck :: (ReverseQueue m) => [EncounterCard] -> m ()
shuffleIntoEncounterDeck = push . ShuffleIntoEncounterDeck

-- | A card matching is found in the encounter deck or discard pile and revealed.
searchAndRevealEncounterCard :: (ReverseQueue m) => IdentityId -> CardMatcher -> m ()
searchAndRevealEncounterCard ident = push . SearchForAndRevealEncounterCard ident

-- | A card matching is found in the encounter deck or discard pile and put into play.
searchAndPutIntoPlayEncounterCard :: (ReverseQueue m) => IdentityId -> CardMatcher -> m ()
searchAndPutIntoPlayEncounterCard ident = push . SearchForAndPutIntoPlayEncounterCard ident

addAccelerationToken :: (ReverseQueue m) => m ()
addAccelerationToken = push AddAccelerationToken

-- | The obligation's own step, once the player may have changed form.
resolveObligation :: (ReverseQueue m) => ObligationId -> IdentityId -> m ()
resolveObligation oid = push . ObligationMessage oid . ResolveObligation

-- | The cards go on top of the encounter deck, the first on top.
putOnTopOfEncounterDeck :: (ReverseQueue m) => [EncounterCard] -> m ()
putOnTopOfEncounterDeck = push . PutOnTopOfEncounterDeck

dealEncounterCard :: (ReverseQueue m) => IdentityId -> m ()
dealEncounterCard = push . DealEncounterCard

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

endPhase :: (ReverseQueue m) => Phase -> m ()
endPhase = push . EndPhase

beginRound :: (ReverseQueue m) => m ()
beginRound = push BeginRound

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

-- | The main scheme leaves play for the next one.
replaceMainScheme :: (ReverseQueue m) => CardCode -> m ()
replaceMainScheme = push . ReplaceMainScheme

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

allyStep :: (ReverseQueue m) => AllyId -> AllyMessage -> m ()
allyStep ally = push . AllyMessage ally

minionStep :: (ReverseQueue m) => MinionId -> MinionMessage -> m ()
minionStep mid = push . MinionMessage mid

treacheryStep :: (ReverseQueue m) => TreacheryId -> TreacheryMessage -> m ()
treacheryStep tid = push . TreacheryMessage tid

obligationStep :: (ReverseQueue m) => ObligationId -> ObligationMessage -> m ()
obligationStep oid = push . ObligationMessage oid

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

-- | All of the damage the identity is about to take is cancelled.
cancelDamage :: (HasQueue m) => IdentityId -> m ()
cancelDamage ident = cancelMatchingMessage \case
  IdentityMessage ident' (IdentityDamaged _ _) -> ident' == ident
  _ -> False

-- | The boost icons about to be added to the villain's activation are cancelled.
cancelBoostIcons :: (HasQueue m) => VillainId -> m ()
cancelBoostIcons vid = cancelMatchingMessage \case
  VillainMessage vid' (VillainAddBoost _) -> vid' == vid
  _ -> False

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

