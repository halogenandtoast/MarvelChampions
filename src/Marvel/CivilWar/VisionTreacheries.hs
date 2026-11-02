module Marvel.CivilWar.VisionTreacheries where

import Marvel.Prelude
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.LeaderAttachments (massFormMatches)
import Marvel.Damage
import Marvel.Entity
import Marvel.EncounterSet qualified as E
import Marvel.Game.Source
import Marvel.Hood.Helpers (controlledCharacters,controlledCards)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Types
import Marvel.Window qualified as W

data SolarActivation = SolarPending VillainId Bool | SolarRunning VillainId Bool Bool
  deriving stock (Show,Eq,Generic)
  deriving anyclass (ToJSON,FromJSON)

newtype VisionTreachery = VisionTreachery (Attrs Treachery `With` Maybe SolarActivation)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

visionTreacheryDefinitions :: [CardDef]
visionTreacheryDefinitions = [C.cw57051,C.cw57052,C.cw57053]

visionTreacheries :: [TreacheryCard VisionTreachery]
visionTreacheries = map (treachery $ \a -> VisionTreachery (a `With` Nothing)) visionTreacheryDefinitions

instance IsTreachery VisionTreachery where
  toTreacheryAttrs (VisionTreachery (a `With` _)) = a
  treacheryModifiersFor _ target (VisionTreachery (_ `With` Just (SolarRunning vid hero True)))
    | target == VillainRef vid = pure [if hero then AttackModifier 1 else SchemeModifier 1]
  treacheryModifiersFor _ _ _ = pure []

visionForm :: HasGame m => CardDef -> m Bool
visionForm form = leaderOfFamily E.CwVision >>= maybe (pure False) (\vid -> selectAny $ massFormMatches (EnemyVillainId vid) form)

readyCharacter :: HasGame m => CharacterId -> m Bool
readyCharacter = \case
  IdentityCharacter you -> identityMatches UnexhaustedIdentity you
  AllyCharacter aid -> allyMatches UnexhaustedAlly aid
  _ -> pure False

exhaustControlledCharacter :: ReverseQueue m => CharacterId -> m ()
exhaustControlledCharacter = \case
  IdentityCharacter you -> exhaust you
  AllyCharacter aid -> exhaust aid
  _ -> pure ()

instance RunMessage VisionTreachery where
  runMessage msg e@(VisionTreachery (a `With` solar)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "57051" -> leaderOfFamily E.CwVision >>= \case
          Nothing -> pure e
          Just vid -> do
            hero <- identityMatches HeroIdentity you
            if hero then enemyAttacks vid you else villainSchemesAgainst vid you
            pure $ VisionTreachery (a {treacheryResolver = Just you} `With` Just (SolarPending vid hero))
        "57052" -> do
          dense <- visionForm C.cw57046a
          chars <- controlledCharacters you
          targets <- filterM (\c -> (||) <$> readyCharacter c <*> (if dense then characterMatches DamageableCharacter c else pure False)) chars
          chooseOne you [targeting (characterRef c) $ do
            exhaustControlledCharacter c
            cardSpecific a "superdenseDamage" c
            | c <- targets]
          cardSpecific a "superdenseFlipIntangible" ()
          pure e
        "57053" -> do
          intangible <- visionForm C.cw57046b
          upgrades <- if intangible then controlledCards you [UpgradeType]
            else map UpgradeRef <$> selectList (UpgradeControlledBy (IdentityWithId you) <> UnexhaustedUpgrade)
          chooseForEnemyTeam you a [targeting target $ case target of
            UpgradeRef uid -> if intangible then discardControlled target else exhaust uid
            _ -> pure ()
            | target <- upgrades]
          cardSpecific a "phaseDisruptionFlipDense" ()
          pure e
        _ -> pure e
    CheckWindows windows | Just (SolarPending vid hero) <- solar -> do
      let starts (W.Window W.When event) = case event of
            W.EnemyAttack (EnemyVillainId other) _ -> hero && other == vid
            W.EnemyAttackAlly (EnemyVillainId other) _ _ -> hero && other == vid
            W.EnemyAttackOnEnemy (EnemyVillainId other) _ -> hero && other == vid
            W.Schemed other -> not hero && other == vid
            _ -> False
          starts _ = False
      if any starts windows then do
        matches <- visionForm $ if hero then C.cw57046a else C.cw57046b
        pure $ VisionTreachery (a `With` Just (SolarRunning vid hero matches))
      else pure e
    CardSpecific (isTarget a -> True) "superdenseDamage" (fromJSON -> Success (character :: CharacterId)) -> do
      dense <- visionForm C.cw57046a
      when dense $ damageCharacter character a (toDamage 2 FromAbility)
      pure e
    CardSpecific (isTarget a -> True) key _ | key `elem` ["superdenseFlipIntangible","phaseDisruptionFlipDense"] -> do
      let form = if key == "superdenseFlipIntangible" then C.cw57046b else C.cw57046a
      matching <- visionForm form
      when matching $ leaderOfFamily E.CwVision >>= traverse_ (flipLeaderMassForm . EnemyVillainId)
      pure e
    VillainMessage vid step | Just (SolarRunning host _ _) <- solar
      , vid == host
      , step == VillainEndAttack || step == VillainSchemeAgainstEnded -> pure $ VisionTreachery (a `With` Nothing)
    _ -> (\updated -> VisionTreachery (updated `With` solar)) <$> runMessage msg a
