module Marvel.CivilWar.SpiderMan where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.CloakAndDagger (removeFriendlyThreat)
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype SpiderMan = SpiderMan (Attrs Minion `With` Bool)
  deriving anyclass (HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

instance IsMinion SpiderMan where
  toMinionAttrs (SpiderMan (a `With` _)) = a
  overMinionAttrs f (SpiderMan (a `With` attacked)) = SpiderMan (f a `With` attacked)

spiderMan :: MinionCard SpiderMan
spiderMan = minion (\a -> SpiderMan (a `With` False)) C.cw56180 (Sch 2) (Atk 2) (HP 4)

instance RunMessage SpiderMan where
  runMessage msg e@(SpiderMan (a `With` attacked)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      attachments <- inAnyArea $ selectList $ AttachmentIs C.cw56181
      characters <- nub . catMaybes <$> traverse getAttachedCharacter attachments
      previous <- getActivePlayerId
      enemyActivationAfterCurrent mid do
        for_ characters $ \character -> case character of
          IdentityCharacter iid -> setActivePlayer iid >> enemyAttacks mid iid
          VillainCharacter vid -> do
            first <- firstPlayerAtLeader vid
            setActivePlayer first
            minionStep mid (MinionAttacksEnemy $ EnemyVillainId vid)
          MinionCharacter target -> setActivePlayer you >> minionStep mid (MinionAttacksEnemy $ EnemyMinionId target)
          AllyCharacter aid -> getAllyControllerId aid >>= traverse_ (\owner -> setActivePlayer owner >> minionStep mid (MinionAttacksAlly owner aid))
          AttachmentCharacter _ -> pure ()
        setActivePlayer previous
        cardSpecific a "afterTangledAttacks" you
      pure $ SpiderMan (a `With` False)
    CheckWindows windows -> do
      a' <- runMessage msg a
      let madeAttack = any (\case
            W.Window W.When (W.EnemyAttack (EnemyMinionId mid) _) -> mid == minionId a
            W.Window W.When (W.EnemyAttackAlly (EnemyMinionId mid) _ _) -> mid == minionId a
            W.Window W.When (W.EnemyAttackOnEnemy (EnemyMinionId mid) _) -> mid == minionId a
            _ -> False) windows
      pure $ SpiderMan (a' `With` (attacked || madeAttack))
    CardSpecific (isTarget a -> True) "afterTangledAttacks" (fromJSON -> Success you) -> do
      unless attacked $ do
        deck <- asYou you getEncounterDeck
        discard <- asYou you getEncounterDiscard
        key <- getDeckKeyFor you
        let cards = filter ((== "56181") . toCardCode) (deck <> discard)
        lookForEnemyTeam you a cards [cardLabeled (toCardCode card) (dealFoundEncounterCard you card) | card <- cards]
        fromDeck key shuffleEncounterDeck
      pure e
    _ -> (\a' -> SpiderMan (a' `With` attacked)) <$> runMessage msg a

newtype TangledUp = TangledUp (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

tangledUp :: AttachmentCard TangledUp
tangledUp = attachment TangledUp C.cw56181

instance HasModifiersFor TangledUp where
  getModifiersFor _ (IdentityRef iid) (TangledUp a) | Just iid == attachmentIdentity a = pure [ReplacePowerWithDiscard $ toRef a]
  getModifiersFor _ _ _ = pure []

instance HasAbilities TangledUp where
  getAbilities (TangledUp a) =
    [windowAbility a idx window ForcedInterrupt NoCost (runAbility a idx)
    | Just enemy <- [attachmentEnemy a]
    , (idx,window) <- [(1,W.EnemyAttackedAnyTarget W.Would $ EnemyWithId enemy),(2,W.EnemyWouldScheme (EnemyWithId enemy) AnyIdentity)]]

instance RunMessage TangledUp where
  runMessage msg e@(TangledUp a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      leader <- friendlyLeaderFor you
      eligible <- filterM (canAttach . toTarget) [vid | Just vid <- [leader]]
      case eligible of
        vid : _ -> attachTo aid vid
        [] -> do
          hero <- asYou you $ identityMatches HeroIdentity you
          allowed <- canAttach $ IdentityRef you
          if hero && allowed then attachToIdentity aid you else removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) idx _ _ | idx `elem` [1,2] -> do
      for_ (attachmentEnemy a) $ \enemy -> if idx == 1 then cancelEnemyActivation enemy else cancelPendingEnemyScheme enemy
      removeFromPlay a
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      present <- asYou you $ selectAny $ MinionIs C.cw56180
      when present $ revealBoost a you
      pure e
    _ -> TangledUp <$> runMessage msg a
   where
    canAttach target = do
      modifiers <- getModifiers target
      pure $ all (`notElem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]

newtype Spectacular = Spectacular (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

spectacular :: TreacheryCard Spectacular
spectacular = treachery Spectacular C.cw56182

instance RunMessage Spectacular where
  runMessage msg e@(Spectacular a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      mains <- asYou you $ selectList EncounterMainScheme
      traverse_ (\scheme -> placeThreatOn a scheme 2) mains
      removeFriendlyThreat you a 2
      pure e
    _ -> Spectacular <$> runMessage msg a

newtype NeighborhoodHero = NeighborhoodHero (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

neighborhoodHero :: SideSchemeCard NeighborhoodHero
neighborhoodHero = sideScheme NeighborhoodHero C.cw56183 (Static 2)

instance RunMessage NeighborhoodHero where
  runMessage msg e@(NeighborhoodHero a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      you <- maybe getYou pure (sideSchemeRevealedBy a)
      competition <- getCompetition
      moves <- if isJust competition then do
        leader <- friendlyLeaderFor you
        fmap concat $ for [vid | Just vid <- [leader]] $ \vid -> do
          area <- getAreaOfTarget vid
          first <- firstPlayerAtLeader vid
          mains <- getMainSchemesIn area
          fmap catMaybes $ for mains $ \mid -> do
            let source = SchemeMainSchemeId mid
            allowed <- asYou first $ canMoveSchemeThreatFrom a source
            n <- min 2 <$> getSchemeThreatAmount source
            pure $ if allowed && n > 0 then Just (mid,n) else Nothing
        else pure []
      if null moves then do
        n <- asYou you getPlayerCount
        placeThreatOn a sid (2 * fromIntegral n)
      else for_ moves $ \(mid,n) -> do
        cardSpecific mid "moveThreatOut" n
        cardSpecific a "moveThreatIn" n
      pure e
    _ -> NeighborhoodHero <$> runMessage msg a
