-- | The game log, derived from the message stream, as in Arkham's "Arkham.Log.Narrator".
--
-- Cards do not write the log. Every change to the game passes through a 'Message', so
-- one 'case' here, knowing what each message means, gives the whole log in one place a
-- reader can review, and a new card logs correctly without doing anything.
--
-- Three rules:
--
-- 1. __Silence is the default.__ Most messages are plumbing (windows, asks, payments),
--    and a message nothing here claims says nothing.
-- 2. __The narrator runs before the message.__ It reads the game as the message finds
--    it: a card about to leave play is still there to be named, a deck still holds the
--    cards about to be drawn.
-- 3. __Never break the game.__ It only reads, never pushes, and never assumes something
--    named in a message is still in play.
module Marvel.Log.Narrator (narrationFor) where

import Marvel.Prelude

import Marvel.Ability.Types (Ability (..), AbilityType (..))
import Marvel.Card
import Marvel.Damage
import Marvel.Deck
import Marvel.Entity
import Marvel.Game
import Marvel.Id
import Marvel.Identity
import Marvel.Log
import Marvel.Ally.Types qualified as Ally
import Marvel.Message
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Name
import Marvel.SideScheme.Types qualified as SideScheme
import Marvel.Support.Types qualified as Support
import Marvel.Phase
import Marvel.Question (Choice (ChangeForm))
import Marvel.Ref
import Marvel.Villain.Cards (allVillainsMap)
import Marvel.Scenario.Types qualified as Scenario
import Marvel.Villain.Types qualified as Villain

-- | What a message adds to the log, if anything. The match is pure, so a message that
-- says nothing costs nothing; only a narrating one reads the game.
narrationFor :: (HasGame m) => Message -> Maybe (m [LogEntry])
narrationFor = \case
  -- * Structure: the headings a reader finds their way by
  StartScenario -> Just do
    villains <- getsGame (Scenario.scenarioVillains . toAttrs . gameScenario)
    pure
      [ structure [ikeyPart "log.scenarioBegins" ["villain" ~> (codeRef RefVillain v) {logRefName = maybe (unCardCode v) (display . cdName) (lookup v allVillainsMap)}]]
      | v <- take 1 villains
      ]
  BeginPhase PlayerPhase -> Just do
    n <- getsGame gameRound
    pure [structure [ikeyPart "log.round" ["count" ~> n]], structure [ikeyPart "log.phase.player" []]]
  BeginPhase VillainPhase -> Just $ one $ structure [ikeyPart "log.phase.villain" []]
  GameOver Won -> Just $ one $ toned Good $ structure [ikeyPart "log.gameOver.won" []]
  GameOver Lost -> Just $ one $ toned Bad $ structure [ikeyPart "log.gameOver.lost" []]
  -- * What the players do
  IdentityMessage ident msg -> identityNarration ident msg
  -- changing form is told as the change itself
  UsedAbility _ a | ChangeForm `elem` a.abilityChoices -> Nothing
  UsedAbility ident a | narrates a.abilityType -> Just $ whenRef (refocus a.abilitySource) \source -> do
    who <- playerRef' ident
    one $ action [ikeyPart "log.usesAbility" ["player" ~> who, "card" ~> source]]
  Simultaneously msgs -> case mapMaybe narrationFor msgs of
    [] -> Nothing
    told -> Just $ concat <$> sequence told
  -- * Enemies
  VillainMessage vid msg -> villainNarration vid msg
  MinionMessage mid msg -> minionNarration mid msg
  AllyMessage aid msg -> allyNarration aid msg
  -- * Schemes
  MainSchemeMessage sid msg -> schemeNarration (MainSchemeRef sid) msg
  SideSchemeMessage sid msg -> sideSchemeNarration sid msg
  NextMainScheme -> Just $ one $ toned Bad $ mechanic [ikeyPart "log.mainSchemeAdvances" []]
  -- the active counter moves: told only when it goes to another villain
  SetActiveVillain vid -> Just do
    aid <- getAreaOfTarget vid
    current <- getActiveVillainIn aid
    if current == Just vid then pure [] else whenRef (VillainRef vid) \v -> one $ mechanic [ikeyPart "log.activeVillain" ["villain" ~> v]]
  -- * The encounter deck
  RevealEncounterCard ident card -> Just do
    who <- playerRef' ident
    one $ mechanic [ikeyPart "log.reveals" ["player" ~> who, "card" ~> EncounterCard card]]
  GiveStatus target s -> Just $ simply (statusKey s) target
  RevealBoostCard card _ -> Just $ one $ notice [ikeyPart "log.boost" ["card" ~> EncounterCard card]]
  PlaceCounters target n | n > 0 -> Just $ counted "log.countersPlaced" target n
  RemoveCounters target n | n > 0 -> Just $ counted "log.countersRemoved" target n
  EnvironmentMessage eid FlipEnvironment -> Just $ simply "log.flips" (EnvironmentRef eid)
  AttachmentMessage aid (AttachedToIdentity ident) -> Just $ attachedTo aid (IdentityRef ident)
  AttachmentMessage aid (AttachedToSideScheme sid) -> Just $ attachedTo aid (SideSchemeRef sid)
  -- put into play attached, not revealed: the card is not in play yet to be named
  AttachEncounterCard card enemy -> Just $ whenRef (EnemyRef enemy) \t ->
    one $ toned Bad $ mechanic [ikeyPart "log.attachedTo" ["card" ~> EncounterCard card, "target" ~> t]]
  RemovePlayerObligationsFromGame -> Just $ one $ mechanic [ikeyPart "log.playerObligationsRemoved" []]
  DiscardTopOfEncounterDeck n _ | n > 0 -> Just $ one $ mechanic [ikeyPart "log.encounterCardsDiscarded" ["count" ~> n]]
  -- told once, as it is dealt; each character's share is told as it takes it
  AssignIndirectDamage ident _ n [] | n > 0 -> Just do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.takesIndirectDamage" ["player" ~> who, "count" ~> n]]
  _ -> Nothing
 where
  statusKey = \case
    StunnedStatus -> "log.stunned"
    ConfusedStatus -> "log.confused"
    ToughStatus -> "log.tough"
  -- basic powers are told by what they do; resources by the card they pay for
  narrates = \case
    Basic -> False
    Resource -> False
    HeroResource -> False
    AlterEgoResource -> False
    Setup -> False
    _ -> True

identityNarration :: (HasGame m) => IdentityId -> IdentityMessage -> Maybe (m [LogEntry])
identityNarration ident = \case
  -- a card that was nobody's, as a rescued Captive ally, becomes the player's
  AddToHand card | isJust (pcOwner card) -> Just do
    unowned <- getsGame \g -> any (any (\c -> pcCardId c == pcCardId card && isNothing (pcOwner c)) . SideScheme.sideSchemeHeldCards . toAttrs) (gameSideSchemes g)
    if unowned
      then do
        who <- playerRef' ident
        one $ toned Good $ action [ikeyPart "log.takesControlOf" ["player" ~> who, "card" ~> PlayerCard card]]
      else pure []
  BeginTurn -> Just do
    who <- playerRef' ident
    one $ structure [ikeyPart "log.turn" ["player" ~> who]]
  PlayedCard card _ -> Just do
    who <- playerRef' ident
    one $ action [ikeyPart "log.plays" ["player" ~> who, "card" ~> PlayerCard card]]
  ChangedToForm side -> Just do
    who <- playerRef' ident
    face <- getsGame \g -> do
      p <- lookup ident (gamePlayers g)
      faceDef <$> lookup side (toAttrs p).playerIdentitySides
    pure
      [ action [ikeyPart "log.changesForm" ["player" ~> who, "identity" ~> (codeRef RefIdentity (cdCardCode def)) {logRefName = display (cdName def)}]]
      | Just def <- [face]
      ]
  SideMessage Attacked -> Just $ simplyPlayer "log.basicAttack"
  SideMessage Thwarted -> Just $ simplyPlayer "log.basicThwart"
  SideMessage Recovered -> Just $ simplyPlayer "log.recovers"
  SideMessage (Defended enemy) -> Just $ whenRef (EnemyRef enemy) \e -> do
    who <- playerRef' ident
    one $ action [ikeyPart "log.defends" ["player" ~> who, "enemy" ~> e]]
  DrawCards FromDeck n | n > 0 -> Just do
    who <- playerRef' ident
    drawn <- getsGame $ maybe [] (take (fromIntegral n) . unDeck . (.playerIdentityDeck) . toAttrs) . lookup ident . gamePlayers
    pure
      [ forPlayer ident $ notice [ikeyPart "log.drawsCards" ["player" ~> who, "cards" ~> map PlayerCard drawn]]
      , exceptPlayer ident $ notice [ikeyPart "log.draws" ["player" ~> who, "count" ~> length drawn]]
      ]
  IdentityDamaged _ dmg | damageAmount dmg > 0 -> Just do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.takesDamage" ["target" ~> who, "count" ~> damageAmount dmg]]
  IdentityHealed n | n > 0 -> Just do
    who <- playerRef' ident
    damage <- getsGame $ maybe 0 identityDamage . lookup ident . gamePlayers
    let healed = min n damage
    pure [toned Good $ mechanic [ikeyPart "log.heals" ["target" ~> who, "count" ~> healed]] | healed > 0]
  IdentitySetHitPoints n -> Just do
    who <- playerRef' ident
    one $ toned Good $ mechanic [ikeyPart "log.hitPointsSetTo" ["target" ~> who, "count" ~> n]]
  IdentityDefeated -> Just do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart "log.eliminated" ["player" ~> who]]
  IdentityStunned -> Just $ status "log.stunned"
  IdentityConfused -> Just $ status "log.confused"
  IdentityBecomeTough -> Just $ status "log.tough"
  _ -> Nothing
 where
  simplyPlayer key = do
    who <- playerRef' ident
    one $ action [ikeyPart key ["player" ~> who]]
  status key = do
    who <- playerRef' ident
    one $ toned Bad $ mechanic [ikeyPart key ["target" ~> who]]

villainNarration :: (HasGame m) => VillainId -> VillainMessage -> Maybe (m [LogEntry])
villainNarration vid = \case
  -- told as each gets under way, so one an ability replaces (Norman Osborn's attack, Green
  -- Goblin's scheme) is told as what it is replaced with; a stun or confusion as it is spent
  VillainAttacks ident -> Just $ whenM' (villainHas (.villainStunned) vid) $ activation (VillainRef vid) ident "log.attacks" True
  VillainBeginAttack ident -> Just $ activation (VillainRef vid) ident "log.attacks" False
  VillainBeginAttackWithoutBoost ident -> Just $ activation (VillainRef vid) ident "log.attacks" False
  VillainSchemes -> Just $ whenM' (villainHas (.villainConfused) vid) $ schemes (VillainRef vid) True
  VillainBeginScheme -> Just $ schemes (VillainRef vid) False
  VillainDamaged _ dmg | damageAmount dmg > 0 -> Just do
    tough <- getsGame $ maybe False Villain.villainIsTough . lookup vid . gameVillains
    damaged (VillainRef vid) (tough && preventedByTough dmg) (damageAmount dmg)
  VillainHealed n | n > 0 -> Just $ healedBy (VillainRef vid) n
  VillainDefeated -> Just $ whenRef (VillainRef vid) \v -> one $ toned Good $ mechanic [ikeyPart "log.defeated" ["target" ~> v]]
  VillainAdvanced -> Just $ whenRef (VillainRef vid) \v -> one $ toned Bad $ mechanic [ikeyPart "log.villainAdvances" ["villain" ~> v]]
  FlipVillain -> Just $ simply "log.flips" (VillainRef vid)
  VillainStunned _ -> Just $ simply "log.stunned" (VillainRef vid)
  VillainConfused _ -> Just $ simply "log.confused" (VillainRef vid)
  VillainBecomeTough -> Just $ simply "log.tough" (VillainRef vid)
  -- told as they are added, so icons cancelled as the card turns up are not
  VillainAddBoost n | n > 0 -> Just $ whenRef (VillainRef vid) \v -> one $ notice [ikeyPart "log.boostAdded" ["enemy" ~> v, "count" ~> n]]
  _ -> Nothing

-- | What the villain's state says, as the message finds it.
villainHas :: (HasGame m) => (Attrs Villain.Villain -> Bool) -> VillainId -> m Bool
villainHas f vid = getsGame (maybe False (f . toAttrs) . lookup vid . gameVillains)

whenM' :: (Monad m) => m Bool -> m [LogEntry] -> m [LogEntry]
whenM' cond body = cond >>= \b -> if b then body else pure []

minionNarration :: (HasGame m) => MinionId -> MinionMessage -> Maybe (m [LogEntry])
minionNarration mid = \case
  MinionEngagedIdentity ident -> Just $ whenRef (MinionRef mid) \m -> do
    who <- playerRef' ident
    one $ mechanic [ikeyPart "log.engages" ["minion" ~> m, "player" ~> who]]
  MinionAttacks ident -> Just $ activation (MinionRef mid) ident "log.attacks" =<< getsGame (maybe False ((.minionStunned) . toAttrs) . lookup mid . gameMinions)
  MinionSchemes -> Just $ schemes (MinionRef mid) =<< getsGame (maybe False ((.minionConfused) . toAttrs) . lookup mid . gameMinions)
  MinionDamaged _ dmg | damageAmount dmg > 0 -> Just do
    tough <- getsGame $ maybe False ((.minionTough) . toAttrs) . lookup mid . gameMinions
    damaged (MinionRef mid) (tough && preventedByTough dmg) (damageAmount dmg)
  MinionHealed n | n > 0 -> Just $ healedBy (MinionRef mid) n
  MinionDefeated -> Just $ whenRef (MinionRef mid) \m -> one $ toned Good $ mechanic [ikeyPart "log.defeated" ["target" ~> m]]
  MinionStunned _ -> Just $ simply "log.stunned" (MinionRef mid)
  MinionConfused _ -> Just $ simply "log.confused" (MinionRef mid)
  MinionBecomeTough -> Just $ simply "log.tough" (MinionRef mid)
  MinionAddBoost n | n > 0 -> Just $ whenRef (MinionRef mid) \m -> one $ notice [ikeyPart "log.boostAdded" ["enemy" ~> m, "count" ~> n]]
  _ -> Nothing

allyNarration :: (HasGame m) => AllyId -> AllyMessage -> Maybe (m [LogEntry])
allyNarration aid = \case
  AllyAttacked -> Just $ whenRef (AllyRef aid) \a -> one $ action [ikeyPart "log.allyAttacks" ["ally" ~> a]]
  AllyThwarted -> Just $ whenRef (AllyRef aid) \a -> one $ action [ikeyPart "log.allyThwarts" ["ally" ~> a]]
  AllyDefended enemy -> Just $ whenRef (AllyRef aid) \a -> whenRef (EnemyRef enemy) \e ->
    one $ action [ikeyPart "log.allyDefends" ["ally" ~> a, "enemy" ~> e]]
  AllyDamaged _ dmg | damageAmount dmg > 0 -> Just do
    tough <- getsGame $ maybe False ((.allyTough) . toAttrs) . lookup aid . gameAllies
    damaged (AllyRef aid) (tough && preventedByTough dmg) (damageAmount dmg)
  AllyHealed n | n > 0 -> Just $ healedBy (AllyRef aid) n
  AllyDefeated -> Just $ whenRef (AllyRef aid) \a -> one $ toned Bad $ mechanic [ikeyPart "log.defeated" ["target" ~> a]]
  AllyStunned -> Just $ simply "log.stunned" (AllyRef aid)
  _ -> Nothing

schemeNarration :: (HasGame m) => Target -> MainSchemeMessage -> Maybe (m [LogEntry])
schemeNarration target = \case
  MainSchemePlaceThreat n | n > 0 -> Just $ threat target "log.threatPlaced" n Bad
  MainSchemeThwarted _ n | n > 0 -> Just $ threat target "log.threatRemoved" n Good
  _ -> Nothing

sideSchemeNarration :: (HasGame m) => SideSchemeId -> SideSchemeMessage -> Maybe (m [LogEntry])
sideSchemeNarration sid = \case
  RevealSideScheme -> Just $ simply "log.sideSchemeRevealed" (SideSchemeRef sid)
  SideSchemePlaceThreat n | n > 0 -> Just $ threat (SideSchemeRef sid) "log.threatPlaced" n Bad
  SideSchemeThwarted _ n | n > 0 -> Just $ threat (SideSchemeRef sid) "log.threatRemoved" n Good
  SideSchemeThreatRemoved n | n > 0 -> Just $ threat (SideSchemeRef sid) "log.threatRemoved" n Good
  DefeatSideScheme -> Just $ whenRef (SideSchemeRef sid) \s -> one $ toned Good $ mechanic [ikeyPart "log.defeated" ["target" ~> s]]
  _ -> Nothing

-- * Pieces

one :: (Applicative m) => LogEntry -> m [LogEntry]
one = pure . pure

simply :: (HasGame m) => Text -> Target -> m [LogEntry]
simply key target = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t]]

whenRef :: (HasGame m) => Target -> (LogRef -> m [LogEntry]) -> m [LogEntry]
whenRef target f = maybe (pure []) f =<< refFor target

-- | An enemy activating against a player, or shaking off the status that stops it.
activation :: (HasGame m) => Target -> IdentityId -> Text -> Bool -> m [LogEntry]
activation enemy ident key blocked = whenRef enemy \e -> do
  who <- playerRef' ident
  one $
    if blocked
      then toned Good $ mechanic [ikeyPart "log.stunnedInstead" ["enemy" ~> e]]
      else mechanic [ikeyPart key ["enemy" ~> e, "player" ~> who]]

schemes :: (HasGame m) => Target -> Bool -> m [LogEntry]
schemes enemy confused = whenRef enemy \e ->
  one $
    if confused
      then toned Good $ mechanic [ikeyPart "log.confusedInstead" ["enemy" ~> e]]
      else mechanic [ikeyPart "log.schemes" ["enemy" ~> e]]

-- | Damage, the tough status it takes off instead, or none at all for a character that
-- cannot take damage (Red Skull, while The Red House is in play).
damaged :: (HasGame m) => Target -> Bool -> Natural -> m [LogEntry]
damaged target tough n = whenRef target \t -> do
  prevented <- elem CannotTakeDamage <$> getModifiers target
  one $
    if
      | prevented -> toned Good $ mechanic [ikeyPart "log.takesNoDamage" ["target" ~> t]]
      | tough -> mechanic [ikeyPart "log.toughRemoved" ["target" ~> t]]
      | otherwise -> mechanic [ikeyPart "log.takesDamage" ["target" ~> t, "count" ~> n]]

attachedTo :: (HasGame m) => AttachmentId -> Target -> m [LogEntry]
attachedTo aid target = whenRef (AttachmentRef aid) \a -> whenRef target \t ->
  one $ toned Bad $ mechanic [ikeyPart "log.attachedTo" ["card" ~> a, "target" ~> t]]

counted :: (HasGame m) => Text -> Target -> Natural -> m [LogEntry]
counted key target n = whenRef target \t -> one $ mechanic [ikeyPart key ["target" ~> t, "count" ~> n]]

healedBy :: (HasGame m) => Target -> Natural -> m [LogEntry]
healedBy target n = whenRef target \t -> one $ mechanic [ikeyPart "log.heals" ["target" ~> t, "count" ~> n]]

threat :: (HasGame m) => Target -> Text -> Natural -> LogTone -> m [LogEntry]
threat target key n tone = whenRef target \t -> one $ toned tone $ mechanic [ikeyPart key ["scheme" ~> t, "count" ~> n]]

-- | A player, as their name and the face they show.
playerRef' :: (HasGame m) => IdentityId -> m LogRef
playerRef' ident =
  getsGame (lookup ident . gamePlayers) <&> \case
    Just p -> (playerRef ident (toAttrs p).playerIdentityName) {logRefCardCode = Just (toCardCode p)}
    Nothing -> playerRef ident ""

faceDef :: PlayerIdentitySide -> CardDef
faceDef = \case
  HeroSide x -> getCardDef x
  AlterEgoSide x -> getCardDef x

codeRef :: LogRefKind -> CardCode -> LogRef
codeRef kind code = (logRef kind (unCardCode code)) {logRefCardCode = Just code}

-- | Something in play, as a chip, if it is still there to be named.
refFor :: (HasGame m) => Target -> m (Maybe LogRef)
refFor = \case
  IdentityRef ident -> getsGame (lookup ident . gamePlayers) >>= traverse \p -> do
    r <- playerRef' ident
    pure r {logRefKind = RefIdentity, logRefName = display (cdName (getCardDef p))}
  VillainRef x -> byId RefVillain x gameVillains getCardDef
  MinionRef x -> byId RefMinion x gameMinions getCardDef
  AllyRef x -> byId RefAlly x gameAllies getCardDef
  SupportRef x -> byId RefSupport x gameSupports ((.supportCardDef) . toAttrs)
  UpgradeRef x -> byId RefUpgrade x gameUpgrades getCardDef
  SideSchemeRef x -> byId RefSideScheme x gameSideSchemes ((.sideSchemeCardDef) . toAttrs)
  MainSchemeRef x -> byId RefMainScheme x gameMainSchemes getCardDef
  TreacheryRef x -> byId RefTreachery x gameTreacheries getCardDef
  AttachmentRef x -> byId RefAttachment x gameAttachments getCardDef
  EnvironmentRef x -> byId RefEnvironment x gameEnvironments getCardDef
  ObligationRef x -> byId RefObligation x gameObligations getCardDef
  EnemyRef (EnemyVillainId x) -> refFor (VillainRef x)
  EnemyRef (EnemyMinionId x) -> refFor (MinionRef x)
  SchemeRef (SchemeMainSchemeId x) -> refFor (MainSchemeRef x)
  SchemeRef (SchemeSideSchemeId x) -> refFor (SideSchemeRef x)
  CharacterRef c -> refFor case c of
    IdentityCharacter x -> IdentityRef x
    AllyCharacter x -> AllyRef x
    VillainCharacter x -> VillainRef x
    MinionCharacter x -> MinionRef x
  _ -> pure Nothing
 where
  byId :: (HasGame m, ToJSON i, Hashable i) => LogRefKind -> i -> (Game -> HashMap i a) -> (a -> CardDef) -> m (Maybe LogRef)
  byId kind x entities def = fmap (entityRef kind x . def) <$> getsGame (lookup x . entities)

