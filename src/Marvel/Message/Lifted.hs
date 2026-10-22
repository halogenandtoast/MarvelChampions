-- | What a card does, said as what it does: a handler under 'runQueueT' calls these in
-- the order the card says, and never pushes a message itself. What the game element is
-- given as is any id that names it ('IsEnemy', 'IsCharacter', 'IsScheme', 'IsRef').
--
-- A card imports this in place of "Marvel.Message", "Marvel.Choice" and "Marvel.Queue".
module Marvel.Message.Lifted (
  module Marvel.Message.Lifted,
  module X,
) where

import Marvel.Prelude

import Marvel.Attack (Attack (..))
import Marvel.Card.Def (CardDef, CardMatcher)
import Marvel.Card.EncounterCard (EncounterCard)
import Marvel.Card.PlayerCard.Types (PlayerCard)
import Marvel.Card.Side (Side)
import Marvel.Card.Types (Card, IsCard (..))
import Marvel.Choice qualified as Choice
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers.Types
import Marvel.Message as X hiding (cardSpecific, engageMinion, enemyRetaliates)
import Marvel.Message qualified as Msg
import Marvel.Message.Lifted.Queue as X
import Marvel.Question
import Marvel.Queue
import Marvel.Queue as X (cancelMatchingMessage, replaceMatchingMessage)
import Marvel.Ref
import Marvel.Window.Types (ThreatSource (..), Window (..), WindowTiming (When, Would))
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

-- | Ask, unless there is only the one choice: that is made. None is an error.
chooseOrRunOne :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrRunOne you choices = Choice.chooseOrRunOne you =<< sequence choices

-- | A choice offered on the game element, doing what the body does.
targeting :: (ReverseQueue m, IsRef a) => a -> m () -> m Choice
targeting a body = TargetLabel (toTarget a) . pure . Run <$> capture body

-- | A choice by its label (an i18n key under @$label.@), doing what the body does.
labeled :: (ReverseQueue m) => Text -> m () -> m Choice
labeled l body = Label l . pure . Run <$> capture body

-- | The card's ability as a choice: what it does is worked out as it is chosen.
targetingAbility :: (Applicative m, IsRef a) => a -> Natural -> m Choice
targetingAbility a n = pure $ TargetLabel (toTarget a) [runAbility a n]

-- * Damage, threat and status

-- | What the choice does, made by "you".
resolve :: (ReverseQueue m) => Choice -> m ()
resolve choice = do
  you <- getYou
  Choice.pushChoice you choice

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

-- | Damage to each enemy matching.
damageAllEnemies :: (ReverseQueue m, IsRef source) => source -> Damage -> EnemyMatcher -> m ()
damageAllEnemies source damage matcher = resolve $ DamageAllEnemies matcher (toSource source) damage

-- | Damage to each character matching, as one effect.
damageAllCharacters :: (ReverseQueue m, IsRef source) => source -> Damage -> CharacterMatcher -> m ()
damageAllCharacters source damage matcher = resolve $ DamageAllCharacters matcher (toSource source) damage

-- | Damage to the character: an identity's goes through 'damageIdentity'.
damageCharacter :: (ReverseQueue m, IsCharacter character, IsRef source) => character -> source -> Damage -> m ()
damageCharacter character source = resolve . DamageCharacter (toCharacter character) (toSource source)

-- | An attack's damage to the identity, which is not 'damageIdentity''s.
attackDamageIdentity :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> m ()
attackDamageIdentity ident source = push . IdentityMessage ident . IdentityDamaged (toSource source)

-- | The window after an enemy's attack damages the character, if the damage is that.
attackDealtDamageTo :: (ReverseQueue m, IsCharacter character) => Source -> Damage -> character -> m ()
attackDealtDamageTo source damage = pushAll . attackDealtDamage source damage . toCharacter

-- | Damage to an enemy matching, the player choosing which.
chooseDamage :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> EnemyMatcher -> m ()
chooseDamage you source damage = Choice.pushChoice you . ChooseDamage (toSource source) damage

-- | The player chooses an enemy matching, and the target is told it ('ChoseEnemy').
chooseEnemy :: (ReverseQueue m, IsRef target) => IdentityId -> EnemyMatcher -> target -> m ()
chooseEnemy you matcher = Choice.pushChoice you . ChooseEnemy matcher . toTarget

stun :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> m ()
stun enemy = resolve . Stun (EnemyRef $ toEnemy enemy) . toSource

confuse :: (ReverseQueue m, IsEnemy enemy, IsRef source) => enemy -> source -> m ()
confuse enemy = resolve . Confuse (EnemyRef $ toEnemy enemy) . toSource

heal :: (ReverseQueue m, IsCharacter character) => character -> Natural -> m ()
heal character = resolve . Heal (toCharacter character)

thwartScheme :: (ReverseQueue m, IsScheme scheme, IsRef source) => scheme -> source -> Natural -> m ()
thwartScheme scheme source = resolve . ThwartScheme (SchemeRef $ toScheme scheme) (toSource source)

-- | The threat placed on the scheme, past the window for it ('threatPlacement').
placeThreatPastWindow :: (ReverseQueue m, IsScheme scheme) => scheme -> Natural -> m ()
placeThreatPastWindow scheme = push . threatPlacement (toScheme scheme)

-- | Threat removed from a scheme matching, the player choosing which.
removeThreat :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> SchemeMatcher -> m ()
removeThreat you source n = Choice.pushChoice you . RemoveThreat (toSource source) n

-- | Threat a card places on the scheme, past the window for it ('placeThreatOn').
placeThreat :: (ReverseQueue m, IsScheme scheme) => scheme -> Natural -> m ()
placeThreat = placeThreatFrom ThreatFromAbility

-- | Threat placed on the scheme by what the window says, past the window for it.
placeThreatFrom :: (ReverseQueue m, IsScheme scheme) => ThreatSource -> scheme -> Natural -> m ()
placeThreatFrom from (toScheme -> scheme) n =
  pushAll $ [CheckWindows [Window Would $ W.ThreatPlaced from scheme n] | n > 0] <> [threatPlacement scheme n]

-- | Damage to the hero not from an attack, past the window for it ('damageIdentity').
damageHero :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Damage -> m ()
damageHero ident source = pushAll . damageIdentity ident (toSource source)

-- | The retaliate damage dealt to the character that attacked.
retaliate :: (ReverseQueue m, IsRef source) => source -> CharacterId -> Natural -> m ()
retaliate source attacker = pushAll . retaliateDamage (toSource source) attacker

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

-- | More DEF for the hero's defense under way.
addDefense :: (ReverseQueue m) => IdentityId -> Natural -> m ()
addDefense ident = push . IdentityMessage ident . IdentityDefended

-- * Characters

-- | What can be readied.
class Readyable a where
  readied :: a -> Message

instance Readyable IdentityId where
  readied ident = IdentityMessage ident ReadiedIdentity

instance Readyable AllyId where
  readied ally = AllyMessage ally ReadiedAlly

ready :: (ReverseQueue m, Readyable a) => a -> m ()
ready = push . readied

-- | What can be exhausted.
class Exhaustible a where
  exhausted :: a -> Message

instance Exhaustible AllyId where
  exhausted ally = AllyMessage ally Msg.ExhaustedAlly

exhaust :: (ReverseQueue m, Exhaustible a) => a -> m ()
exhaust = push . exhausted

-- | The ally's attack on the enemy, past the window before it.
allyAttackLands :: (ReverseQueue m, IsEnemy enemy) => AllyId -> enemy -> Damage -> m ()
allyAttackLands ally enemy = push . AllyMessage ally . AllyAttackLands (toEnemy enemy)

defeatAlly :: (ReverseQueue m) => AllyId -> m ()
defeatAlly ally = push $ AllyMessage ally AllyDefeated

exhaustIdentity :: (ReverseQueue m) => IdentityId -> m ()
exhaustIdentity ident = push $ IdentityMessage ident X.ExhaustedIdentity

-- | The hero's basic powers, ATK or THW given the bonus, and the defense applied.
basicAttack, basicThwart :: (ReverseQueue m) => IdentityId -> Natural -> m ()
basicAttack ident = push . IdentityMessage ident . SideMessage . BasicAttack
basicThwart ident = push . IdentityMessage ident . SideMessage . BasicThwart

defenseApplied :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> m ()
defenseApplied ident = push . IdentityMessage ident . SideMessage . DefenseApplied . toEnemy

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

-- | The minion engages the player, leaving the player it was engaged with.
engageMinion :: (ReverseQueue m) => MinionId -> IdentityId -> m ()
engageMinion minion = pushAll . Msg.engageMinion minion

changeToForm :: (ReverseQueue m) => IdentityId -> Side -> m ()
changeToForm ident = push . IdentityMessage ident . ChangedToForm

-- | The character the attack is against is told it was attacked.
wasAttacked :: (ReverseQueue m) => Attack -> m ()
wasAttacked a = push case attackCharacter a of
  IdentityCharacter ident -> IdentityMessage ident (IdentityWasAttacked a)
  AllyCharacter ally -> AllyMessage ally (AllyWasAttacked a)
  _ -> error "Invalid damage target"

declareDefense :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> enemy -> DefensePriority -> m ()
declareDefense ident enemy = push . DeclareDefense ident (toEnemy enemy)

-- * Villains and schemes

-- | A step of the villain's own, for its runner to answer.
villainStep :: (ReverseQueue m) => VillainId -> VillainMessage -> m ()
villainStep vid = push . VillainMessage vid

advanceVillain :: (ReverseQueue m) => VillainId -> m ()
advanceVillain vid = villainStep vid VillainAdvanced

defeatVillain :: (ReverseQueue m) => VillainId -> m ()
defeatVillain vid = villainStep vid VillainDefeated

changeVillainHitPoints :: (ReverseQueue m) => VillainId -> Int -> m ()
changeVillainHitPoints vid = villainStep vid . VillainHitPointsChanged

-- | A boost card dealt facedown to the enemy.
dealBoost :: (ReverseQueue m, IsRef enemy) => enemy -> m ()
dealBoost = push . DealBoost . toTarget

revealBoostCard :: (ReverseQueue m, IsEnemy enemy) => EncounterCard -> enemy -> m ()
revealBoostCard card = push . RevealBoostCard card . toEnemy

clearBoosts :: (ReverseQueue m) => m ()
clearBoosts = push ClearBoosts

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

gameOver :: (ReverseQueue m) => FinishedStatus -> m ()
gameOver = push . GameOver

-- * Encounter cards

putTopOfDeckIntoPlayAsDrone :: (ReverseQueue m) => IdentityId -> m ()
putTopOfDeckIntoPlayAsDrone = push . PutTopOfDeckIntoPlayAsDrone

putEncounterCardIntoPlay :: (ReverseQueue m) => IdentityId -> EncounterCard -> m ()
putEncounterCardIntoPlay ident = push . PutEncounterCardIntoPlay ident

revealEncounterCard :: (ReverseQueue m) => IdentityId -> EncounterCard -> m ()
revealEncounterCard ident = push . RevealedEncounterCard ident

shuffleEncounterDeck :: (ReverseQueue m) => m ()
shuffleEncounterDeck = push ShuffleEncounterDeck

discardTopOfEncounterDeck :: (ReverseQueue m) => Natural -> m ()
discardTopOfEncounterDeck n = push $ DiscardTopOfEncounterDeck n Nothing

-- | Cards are discarded from the zone until one matches, the target told which
-- ('WithDiscardedMatch').
discardUntil :: (ReverseQueue m, IsRef target) => FromZone -> CardMatcher -> target -> m ()
discardUntil zone matcher = push . DiscardUntil zone matcher . toTarget

-- * Cards

drawCards :: (ReverseQueue m) => IdentityId -> Natural -> m ()
drawCards ident = push . IdentityMessage ident . DrawCards FromDeck

discardFromHand :: (ReverseQueue m) => IdentityId -> Natural -> m ()
discardFromHand ident n = push $ IdentityMessage ident $ DiscardFrom FromHand n Nothing

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

-- | The top cards of the player's deck are discarded.
discardTopOfDeck :: (ReverseQueue m) => IdentityId -> Natural -> m ()
discardTopOfDeck ident n = push $ IdentityMessage ident $ DiscardFrom FromDeck n Nothing

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


-- | The top cards of the encounter deck are looked at, the target told which
-- ('SearchFoundCards').
lookAtTopOfEncounterDeck :: (ReverseQueue m, IsRef target) => Natural -> target -> m ()
lookAtTopOfEncounterDeck n = push . LookAtTopOfEncounterDeck n . toTarget

-- | The cards go on top of the encounter deck, the first on top.
putOnTopOfEncounterDeck :: (ReverseQueue m) => [EncounterCard] -> m ()
putOnTopOfEncounterDeck = push . PutOnTopOfEncounterDeck

-- * Effects and steps

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

checkWindows :: (ReverseQueue m) => [Window] -> m ()
checkWindows = push . CheckWindows

-- * Encounter cards

-- | Damage to the character as a card deals it: to a hero or alter-ego through
-- 'damageIdentity'.
damageCharacter :: (ReverseQueue m, IsCharacter character, IsRef source) => character -> source -> Damage -> m ()
damageCharacter character source = resolve . DamageCharacter (toCharacter character) (toSource source)

-- | Threat placed on each scheme matching, by the card.
placeThreatMatching :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> SchemeMatcher -> m ()
placeThreatMatching you source n = Choice.pushChoice you . PlaceThreat (toSource source) n

confuseIdentity :: (ReverseQueue m) => IdentityId -> m ()
confuseIdentity ident = push $ IdentityMessage ident IdentityConfused

stunAlly :: (ReverseQueue m) => AllyId -> m ()
stunAlly ally = push $ AllyMessage ally AllyStunned

exhaustAlly :: (ReverseQueue m) => AllyId -> m ()
exhaustAlly ally = push $ AllyMessage ally Msg.ExhaustedAlly

-- | The villain or minion is given a tough status card.
enemyBecomesTough :: (ReverseQueue m, IsEnemy enemy) => enemy -> m ()
enemyBecomesTough enemy = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid VillainBecomeTough
  EnemyMinionId mid -> MinionMessage mid MinionBecomeTough

-- | All of the minion's damage is healed.
healAllDamage :: (ReverseQueue m) => MinionId -> m ()
healAllDamage mid = push $ MinionMessage mid MinionHealAllDamage

surge :: (ReverseQueue m) => IdentityId -> m ()
surge = push . Surge

dealEncounterCard :: (ReverseQueue m) => IdentityId -> m ()
dealEncounterCard = push . DealEncounterCard

-- | Cards discarded from the player's hand at random.
discardAtRandom :: (ReverseQueue m) => IdentityId -> Natural -> m ()
discardAtRandom ident n = push $ IdentityMessage ident $ DiscardFrom RandomFromHand n Nothing

-- | The top cards of the encounter deck are discarded, the target told which
-- ('WithDiscarded').
discardTopOfEncounterDeckFor :: (ReverseQueue m, IsRef target) => Natural -> target -> m ()
discardTopOfEncounterDeckFor n = push . DiscardTopOfEncounterDeck n . Just . toTarget

-- | The boost card stays in play, under the player.
putBoostIntoPlay :: (ReverseQueue m, IsRef card) => card -> IdentityId -> m ()
putBoostIntoPlay card = push . PutBoostIntoPlay (toTarget card)

-- | More boost icons for the villain's activation.
addBoost :: (ReverseQueue m) => VillainId -> Natural -> m ()
addBoost vid = push . VillainMessage vid . VillainAddBoost

villainAttackGainsOverkill :: (ReverseQueue m) => VillainId -> m ()
villainAttackGainsOverkill vid = push $ VillainMessage vid VillainAttackGainOverkill

-- | The attachment is attached to the enemy ('AttachedToEnemy').
attachTo :: (ReverseQueue m, IsEnemy enemy) => AttachmentId -> enemy -> m ()
attachTo attachment = push . AttachmentMessage attachment . AttachedToEnemy . toEnemy

-- | The enemy is told the attachment is attached to it.
attachedTo :: (ReverseQueue m, IsEnemy enemy) => enemy -> AttachmentId -> m ()
attachedTo enemy attachment = push case toEnemy enemy of
  EnemyVillainId vid -> VillainMessage vid (AttachedToVillain attachment)
  EnemyMinionId mid -> MinionMessage mid (AttachedToMinion attachment)

attachToEnvironment :: (ReverseQueue m) => EnvironmentId -> AttachmentId -> m ()
attachToEnvironment environment = push . EnvironmentMessage environment . AttachedToEnvironment

damageAttachment :: (ReverseQueue m) => AttachmentId -> Natural -> m ()
damageAttachment attachment = push . AttachmentMessage attachment . AttachmentDamaged

-- * Minions

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
