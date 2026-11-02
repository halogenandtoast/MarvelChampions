module Marvel.CivilWar.SignatureAttachments where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.Types qualified as CW
import Marvel.Cost
import Marvel.Entity
import Marvel.EncounterSet qualified as E
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Window qualified as W

newtype SignatureAttachment = SignatureAttachment (Attrs Attachment `With` Maybe EnemyId)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

signatureDefinitions :: [CardDef]
signatureDefinitions = [C.cw56098,C.cw56143,C.cw56174,C.cw57007,C.cw57046a,C.cw57046b]

signatureAttachments :: [AttachmentCard SignatureAttachment]
signatureAttachments = map (attachment $ \a -> SignatureAttachment (a `With` Nothing)) signatureDefinitions

instance IsAttachment SignatureAttachment where
  toAttachmentAttrs (SignatureAttachment (a `With` _)) = a

hostCharacter :: Attrs Attachment -> Maybe CharacterId
hostCharacter a = (toCharacter <$> attachmentEnemy a) <|> (IdentityCharacter <$> attachmentIdentity a) <|> (AllyCharacter <$> attachmentAlly a)

instance HasModifiersFor SignatureAttachment where
  getModifiersFor _ target (SignatureAttachment (a `With` _))
    | Just target == (characterRef <$> hostCharacter a) = pure $ case toCardCode a of
        "56143" -> [KeywordModifier $ Retaliate 1,AttacksAgainstLoseKeyword Piercing]
        "57007" -> [AttackModifier 2]
        "57046a" -> [AttackModifier 1]
        "57046b" -> [SchemeModifier 1]
        _ -> []
  getModifiersFor _ _ _ = pure []

instance HasAbilities SignatureAttachment where
  getAbilities (SignatureAttachment (a `With` pending)) =
    [windowAbility a 2 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 2) | toCardCode a == "56143", Just enemy <- [pending]] <> case toCardCode a of
    "56098" -> [windowAbility a 1 (W.EnemyWasAttacked W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 1) | Just enemy <- [attachmentEnemy a]]
    "56143" -> [windowAbility a 1 (shieldWindow a host) ForcedResponse NoCost (runAbility a 1) | Just host <- [hostCharacter a]]
    "56174" -> [windowAbility a 1 (W.WindowOneOf [W.TreacheryRevealed W.When AnyTreachery source | source <- revealSources]) ForcedInterrupt NoCost (runAbility a 1) | attachmentCounters a < 3]
    "57007" -> [windowAbility a 1 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 1) | Just enemy <- [attachmentEnemy a]]
    "57046b" -> [windowAbility a 1 (W.VillainWouldTakeDamageFrom (VillainWithId vid) AnyDamage) ForcedInterrupt NoCost (runAbility a 1) | Just (EnemyVillainId vid) <- [attachmentEnemy a]]
    _ -> []

shieldWindow :: Attrs Attachment -> CharacterId -> W.WindowMatcher
shieldWindow a _ = W.WindowOneOf
  [W.EnemyDamagedUsingATK AnyIdentity (EnemyWithId enemy) | Just enemy <- [attachmentEnemy a]]

revealSources :: [W.RevealSource]
revealSources = [W.RevealedFromEncounterDeck,W.RevealedFromDiscardPile,W.RevealedFromSetAside,W.RevealedFromRemovedFromGame,W.RevealedFromVillain]

instance RunMessage SignatureAttachment where
  runMessage msg e@(SignatureAttachment (a `With` pending)) = runQueueT $ case msg of
    CheckWindows windows | toCardCode a == "56143" -> do
      attackers <- filterM (\enemy -> anyM (\def -> enemyMatches (EnemyIs def) enemy) [def | def <- C.civilWarDefinitions, cdCardType def == VillainType, cdEncounterSet def == Just E.CwCaptainAmerica])
        [enemy | W.Window W.After (W.EnemyAttacksAndDamages enemy host n) <- windows, n > 0, Just host == hostCharacter a]
      pure $ SignatureAttachment (a `With` (headMay attackers <|> pending))
    VillainMessage vid VillainEndAttack | pending == Just (EnemyVillainId vid) -> pure $ SignatureAttachment (a `With` Nothing)
    RanAbility _ (isTarget a -> True) 2 _ _ | toCardCode a == "56143" -> do
      traverse_ (attachToCharacter (attachmentId a) . toCharacter) pending
      pure $ SignatureAttachment (a `With` Nothing)
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      let family = case toCardCode a of
            "56098" -> E.CwCaptainMarvel
            "56143" -> E.CwCaptainAmerica
            "56174" -> E.CwSpiderWoman
            "57007" -> E.CwSheHulk
            _ -> E.CwVision
      competitive <- getCompetition
      chooser <- case (competitive,hostCharacter a) of
        (Just c,Just host) -> do
          area <- getAreaOfTarget (characterRef host)
          pure $ CW.teamFirstPlayer c $ if area == CW.teamArea CW.Registration then CW.Registration else CW.Resistance
        _ -> pure you
      asYou chooser (leaderOfFamily family) >>= \case
        Just vid -> attachTo aid vid
        Nothing -> if isPermanentAttachment a then removeFromGame a else removeFromPlay a
      pure e
    AttachmentMessage aid (AttachedToEnemy enemy) | aid == attachmentId a -> do
      updated <- runMessage msg a
      when (toCardCode a == "56143" && attachmentEnemy a /= attachmentEnemy updated && attachmentEnemy updated == Just enemy) $
        for_ [vid | EnemyVillainId vid <- [enemy]] $ \vid -> cardSpecific vid "cwShieldAttached" ()
      pure $ SignatureAttachment (updated `With` pending)
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "56098" -> do
          let candidates = [attacker | W.PlayerAttackResolved source you _ <- windows, let attacker = case source of AllyRef aid -> AllyCharacter aid; _ -> IdentityCharacter you]
                <> [VillainCharacter vid | W.EnemyAttackOnEnemy (EnemyVillainId vid) _ <- windows]
          for_ (take 1 candidates) $ \attacker -> do
            placeCounters a 1
            cardSpecific a "cwSpendEnergy" attacker
        "56143" -> do
          let attackers = [IdentityCharacter you | W.AttackUsingATKDamaged you enemies <- windows, Just enemy <- [attachmentEnemy a], enemy `elem` enemies]
                <> [toCharacter enemy | W.EnemyAttacksAndDamages enemy _ _ <- windows]
          for_ (take 1 attackers) $ attachToCharacter (attachmentId a)
        "56174" -> pure ()
        "57007" -> for_ (attachmentEnemy a) $ \enemy -> do
          attackGains enemy Overkill
          afterMatching (\case VillainMessage vid VillainEndAttack -> enemy == EnemyVillainId vid; _ -> False) (removeFromPlay a)
        "57046b" -> for_ (attachmentEnemy a) $ \enemy -> preventEnemyDamage enemy 1
        _ -> pure ()
      pure $ SignatureAttachment ((if toCardCode a == "56174" then a {attachmentCounters = min 3 $ attachmentCounters a + 1} else a) `With` pending)
    CardSpecific (isTarget a -> True) "cwEnergy" (fromJSON -> Success n) -> pure $ SignatureAttachment (a {attachmentCounters = attachmentCounters a + n} `With` pending)
    PlaceCounters target n | isTarget a target,toCardCode a == "56174" ->
      pure $ SignatureAttachment (a {attachmentCounters = min 3 $ attachmentCounters a + n} `With` pending)
    CardSpecific (isTarget a -> True) "cwSpendEnergy" (fromJSON -> Success attacker) -> do
      when (attachmentCounters a >= 4) $ for_ [vid | Just (EnemyVillainId vid) <- [attachmentEnemy a]] $ \vid -> do
        present <- inAnyArea $ selectAny $ CharacterWithId attacker
        when present $ do
          you <- case attacker of
            IdentityCharacter ident -> pure ident
            AllyCharacter aid -> fromMaybe <$> getActivePlayerId <*> getSourceController (AllyRef aid)
            VillainCharacter other -> do
              competition <- getCompetition
              team <- leaderBelongsTo other
              maybe getActivePlayerId pure $ CW.teamFirstPlayer <$> competition <*> team
            _ -> getActivePlayerId
          villainAttacksCharacterGaining vid you attacker [Overkill]
      pure $ SignatureAttachment ((if attachmentCounters a >= 4 then a {attachmentCounters = 0} else a) `With` pending)
    CardSpecific (isTarget a -> True) "cwFlipMass" _ -> pure $ SignatureAttachment (a
      {attachmentCardDef = if toCardCode a == "57046a" then C.cw57046b else C.cw57046a} `With` pending)
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "57007" -> do
      revealBoost a =<< getBoostedPlayer enemy
      pure e
    _ -> (\updated -> SignatureAttachment (updated `With` pending)) <$> runMessage msg a
