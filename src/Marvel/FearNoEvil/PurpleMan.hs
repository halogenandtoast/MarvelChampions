module Marvel.FearNoEvil.PurpleMan where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Attachments.AllyAsMinion
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types hiding (Field(..))
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Treachery.Types
import Marvel.Villain.Types qualified as V
import Marvel.Window qualified as W

newtype PurpleMan = PurpleMan (Attrs V.Villain)
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)
  deriving anyclass (V.IsVillain,HasAbilities)
purpleManStages :: [V.VillainCard PurpleMan]
purpleManStages = [V.villainWith PurpleMan def (Sch sch) (Atk atk) (HP $ PerPlayer hp) (V.stageL .~ stage)
  | (def,sch,atk,hp,stage) <- [(Cards.street60097,1,0,13,1),(Cards.street60098,2,1,15,2),(Cards.street60099,3,1,17,3)]]
instance HasModifiersFor PurpleMan where
  getModifiersFor _ (MinionRef mid) (PurpleMan a) = do
    influenced <- minionMatches (MinionWithTrait T.Influenced) mid
    pure [KeywordModifier keyword | influenced,keyword <- [Guard] <> [Villainous | V.villainStage a >= 2] <> [Patrol | V.villainStage a == 3]]
  getModifiersFor _ _ _ = pure []
instance RunMessage PurpleMan where
  runMessage msg e@(PurpleMan a) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == V.villainId a -> do
      expert <- (== Expert) <$> getDifficulty
      case V.villainStage a of
        1 -> replaceVillain vid "60098" Advanced >> pure e
        2 | expert -> replaceVillain vid "60099" Advanced >> pure e
        _ -> PurpleMan <$> runMessage msg a
    RevealedAsBoost target enemy -> do
      blanked <- isBlanked a
      unless blanked $ do
        card <- getBoostCard target
        for_ [c | Just c <- [card],cdEncounterSet (getCardDef c) == Just Set.FnePurpleMan] $ \c -> do
          you <- getBoostedPlayer enemy
          consumeBoostCard c
          enemyActivationAfterCurrent enemy $ dealFoundEncounterCard you c
      pure e
    _ -> PurpleMan <$> runMessage msg a

newtype Converted = Converted (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsAttachment,HasAbilities)
converted :: AttachmentCard Converted
converted = attachment Converted Cards.street60100
instance HasModifiersFor Converted where
  getBlankingModifiersFor _ (MinionRef mid) (Converted a) = pure [Blank | attachedAllyMinion a == Just mid]
  getBlankingModifiersFor _ _ _ = pure []
  getModifiersFor _ target (Converted a) = do
    traits <- allyMinionTrait T.Influenced a target
    pure $ traits <> [Blank | case target of MinionRef mid -> attachedAllyMinion a == Just mid; _ -> False]
instance RunMessage Converted where
  runMessage msg e@(Converted a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      candidates <- filterM (\ally -> all (`notElem` [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]) <$> getModifiers (AllyRef ally)) =<< selectList (AllyControlledBy $ IdentityWithId you)
      scored <- for candidates $ \ally -> (ally,) . maybe 0 (fromMaybe 0 . cdCost) <$> getTargetCardDef (AllyRef ally)
      if null scored then removeFromPlay a >> surge you else do
        let highest = maximum $ map snd scored
        chooseOne you [targeting (AllyRef ally) do
          allyBecomesMinion ally
          let mid = MinionId $ unAllyId ally
          attachTo aid (EnemyMinionId mid)
          minionStep mid MinionCheckHitPoints | (ally,cost) <- scored,cost == highest]
      pure e
    _ -> revertsToAlly a msg >> (Converted <$> runMessage msg a)

newtype InfluencedMinion = InfluencedMinion (Attrs Minion)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsMinion
influencedMinions :: [MinionCard InfluencedMinion]
influencedMinions = [minion InfluencedMinion def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp) <-
  [(Cards.street60101,1,1,3),(Cards.street60102,3,0,2),(Cards.street60103,0,2,4),(Cards.street60104,1,2,2)]]
instance HasModifiersFor InfluencedMinion where
  getModifiersFor _ target (InfluencedMinion a) = pure [KeywordModifier Piercing | target == toRef a,toCardCode a == "60101"]
instance HasAbilities InfluencedMinion where
  getAbilities (InfluencedMinion a) = [windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId a) WhenRevealed NoCost (runAbility a 1)]
canDiscard :: HasGame m => Target -> m Bool
canDiscard target = do
  def <- getTargetCardDef target
  modifiers <- getModifiers target
  pure $ isJust def && maybe False (notElem Permanent . cdKeywords) def && all (`notElem` modifiers) [CannotBeDiscarded,CannotLeavePlay,CannotLeavePlayByCardAbilities]
instance RunMessage InfluencedMinion where
  runMessage msg e@(InfluencedMinion a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "60101" -> do
          supports <- selectList $ SupportControlledBy AnyIdentity
          upgrades <- selectList $ UpgradeControlledBy AnyIdentity
          targets <- filterM canDiscard $ map SupportRef supports <> map UpgradeRef upgrades
          scored <- for targets $ \target -> (target,) . maybe 0 (fromMaybe 0 . cdCost) <$> getTargetCardDef target
          unless (null scored) $ do
            let highest = maximum $ map snd scored
            chooseOne you [targeting target (discardControlled target) | (target,cost) <- scored,cost == highest]
        "60102" -> placeThreatOnEach a 3 EncounterMainScheme
        "60103" -> do
          allies <- filterM (canDiscard . AllyRef) =<< selectList AnyAlly
          scored <- for allies $ \ally -> (ally,) <$> characterAttackValue (AllyCharacter ally)
          unless (null scored) $ do
            let highest = maximum $ map snd scored
            chooseOne you [targeting (AllyRef ally) (removeFromPlay ally) | (ally,atk) <- scored,atk == highest]
        _ -> do
          let sources = [source | W.DefeatedMinion mid _ source _ <- windows,mid == minionId a]
          controller <- maybe (pure Nothing) getSourceController (headMay sources)
          damageIdentity (fromMaybe you controller) a (toDamage 3 FromAbility)
      pure e
    _ -> InfluencedMinion <$> runMessage msg a

-- Commands are obligations in their owner's play area. Their physical command
-- counters and exhausted state are exposed through the obligation view hooks.
data PurpleCommand = PurpleCommand (Attrs Obligation) Natural Bool
  deriving stock (Show,Eq,Generic)
  deriving anyclass (ToJSON,FromJSON,HasModifiersFor)
instance IsRef PurpleCommand where toRef (PurpleCommand a _ _) = toRef a
instance HasCardCode PurpleCommand where toCardCode (PurpleCommand a _ _) = toCardCode a
instance IsObligation PurpleCommand where
  toObligationAttrs (PurpleCommand a _ _) = a
  obligationDisplayCounters (PurpleCommand _ n _) = n
  obligationDisplayExhausted (PurpleCommand _ _ exhausted) = exhausted
purpleCommands :: [ObligationCard PurpleCommand]
purpleCommands = map (obligation (\a -> PurpleCommand a 3 False)) [Cards.street60105,Cards.street60106,Cards.street60107]
commandCharacters :: CharacterMatcher
commandCharacters = CharacterOneOf [IdentityCharacters HeroIdentity,AllyCharacters AnyAlly]
instance HasAbilities PurpleCommand where
  getAbilities (PurpleCommand a n exhausted) = [ability a 1 ForcedAction (SelfMatches (IdentityWithId you) <> effectCriteria) NoCost (runAbility a 1) | Just you <- [obligationOwner a],n > 0,not exhausted]
   where
    effectCriteria = case toCardCode a of
      "60105" -> CharacterExists commandCharacters
      "60106" -> CharacterExists $ CharacterCanGainTough <> EnemyCharacters VillainEnemy
      _ -> SchemeExists EncounterMainScheme
instance RunMessage PurpleCommand where
  runMessage msg e@(PurpleCommand a n exhausted) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> pure $ PurpleCommand a {obligationOwner=Just you} n exhausted
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      case toCardCode a of
        "60105" -> do
          characters <- selectList commandCharacters
          chooseOne you [targeting (characterRef character) (damageCharacter character a $ toDamage 2 FromAbility) | character <- characters]
        "60106" -> withTheVillain you (`giveStatus` ToughStatus)
        _ -> placeThreatOnEach a 2 EncounterMainScheme
      when (n == 1) $ removeFromPlay a
      pure $ PurpleCommand a (subtractNatural 1 n) True
    IdentityMessage you ReadyCards | obligationOwner a == Just you -> pure $ PurpleCommand a n False
    PlaceCounters target amount | isTarget a target -> pure $ PurpleCommand a (n+amount) exhausted
    RemoveCounters target amount | isTarget a target -> do
      let remaining = subtractNatural amount n
      when (remaining == 0) $ removeFromPlay a
      pure $ PurpleCommand a remaining exhausted
    _ -> (\attrs -> PurpleCommand attrs n exhausted) <$> runMessage msg a

newtype PowerOfSuggestion = PowerOfSuggestion (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsSideScheme,HasAbilities)
powerOfSuggestion :: SideSchemeCard PowerOfSuggestion
powerOfSuggestion = sideScheme PowerOfSuggestion Cards.street60108 (Static 2)
instance HasModifiersFor PowerOfSuggestion where
  getModifiersFor _ (MinionRef _) _ = pure [TraitModifier T.Influenced]
  getModifiersFor _ _ _ = pure []
instance RunMessage PowerOfSuggestion where
  runMessage msg (PowerOfSuggestion a) = runQueueT $ PowerOfSuggestion <$> runMessage msg a

newtype SwayTheMasses = SwayTheMasses (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsTreachery
swayTheMasses :: TreacheryCard SwayTheMasses
swayTheMasses = treachery SwayTheMasses Cards.street60109
instance RunMessage SwayTheMasses where
  runMessage msg e@(SwayTheMasses a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      cards <- filter (\card -> cdCardType (getCardDef card) == MinionType && T.Influenced `member` cdTraits (getCardDef card)) <$> getEncounterDiscard
      if null cards then gainSurge a else chooseOne you [cardLabeled (toCardCode card) (revealEncounterCardFrom W.RevealedFromDiscardPile you card) | card <- cards]
      pure e
    _ -> SwayTheMasses <$> runMessage msg a
