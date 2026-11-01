module Marvel.Minion.Minions.MutantGenesis where

import Data.List (find)
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue ()
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Minion.Types
import Marvel.Minion.Cards qualified as Cards
import Marvel.Stats
import Marvel.Hp
import Marvel.Minion.Runner
import Marvel.Ability
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query ()
import Marvel.Projection ()
import Marvel.Question ()
import Marvel.Modifier ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Keyword
import Marvel.Window qualified as W
import Marvel.Identity.Types ()
import Marvel.Deck ()
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Attachment.Types qualified as A ()
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.Villain.Types qualified as V ()


newtype MutantGenesisMinion = MutantGenesisMinion (Attrs Minion `With` (Maybe IdentityId, Maybe (IdentityId, Natural, Natural)))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMinion MutantGenesisMinion where
  toMinionAttrs (MutantGenesisMinion (a `With` _)) = a
  overMinionAttrs f (MutantGenesisMinion (a `With` p)) = MutantGenesisMinion (f a `With` p)

mkMutantGenesisMinion :: Attrs Minion -> MutantGenesisMinion
mkMutantGenesisMinion a = MutantGenesisMinion (a `With` (Nothing, Nothing))

mgAvalanche :: MinionCard MutantGenesisMinion
mgAvalanche = minion mkMutantGenesisMinion Cards.mgAvalanche (Sch 1) (Atk 3) (HP 5)

mgBlob :: MinionCard MutantGenesisMinion
mgBlob = minion mkMutantGenesisMinion Cards.mgBlob (Sch 1) (Atk 2) (HP 6)

mgPyro :: MinionCard MutantGenesisMinion
mgPyro = minion mkMutantGenesisMinion Cards.mgPyro (Sch 2) (Atk 0) (HP 4)

mgToad :: MinionCard MutantGenesisMinion
mgToad = minion mkMutantGenesisMinion Cards.mgToad (Sch 2) (Atk 2) (HP 3)

mgMystique :: MinionCard MutantGenesisMinion
mgMystique = minion mkMutantGenesisMinion Cards.mgMystique (Sch 0) (Atk 0) (HP 9)

mgSentinelMarkIv :: MinionCard MutantGenesisMinion
mgSentinelMarkIv = minion mkMutantGenesisMinion Cards.mgSentinelMarkIv (Sch 2) (Atk 2) (HP 4)

mgSentinelMarkIi :: MinionCard MutantGenesisMinion
mgSentinelMarkIi = minion mkMutantGenesisMinion Cards.mgSentinelMarkIi (Sch 1) (Atk 2) (HP 2)

mgSentinelMarkIii :: MinionCard MutantGenesisMinion
mgSentinelMarkIii = minion mkMutantGenesisMinion Cards.mgSentinelMarkIii (Sch 2) (Atk 3) (HP 3)

mgSentinelMarkV :: MinionCard MutantGenesisMinion
mgSentinelMarkV = minion mkMutantGenesisMinion Cards.mgSentinelMarkV (Sch 1) (Atk 3) (HP 5)

mgSentinelMarkVi :: MinionCard MutantGenesisMinion
mgSentinelMarkVi = minion mkMutantGenesisMinion Cards.mgSentinelMarkVi (Sch 2) (Atk 2) (HP 6)

mgSentinelMarkViii :: MinionCard MutantGenesisMinion
mgSentinelMarkViii = minion mkMutantGenesisMinion Cards.mgSentinelMarkViii (Sch 3) (Atk 3) (HP 8)

mgMTypeSentinel :: MinionCard MutantGenesisMinion
mgMTypeSentinel = minion mkMutantGenesisMinion Cards.mgMTypeSentinel (Sch 2) (Atk 2) (HP 5)

mgFabianCortez :: MinionCard MutantGenesisMinion
mgFabianCortez = minion mkMutantGenesisMinion Cards.mgFabianCortez (Sch 1) (Atk 1) (HP 4)

mgAmeliaVoght :: MinionCard MutantGenesisMinion
mgAmeliaVoght = minion mkMutantGenesisMinion Cards.mgAmeliaVoght (Sch 2) (Atk 1) (HP 5)

mgSenyaka :: MinionCard MutantGenesisMinion
mgSenyaka = minion mkMutantGenesisMinion Cards.mgSenyaka (Sch 1) (Atk 3) (HP 3)

mgDelgado :: MinionCard MutantGenesisMinion
mgDelgado = minion mkMutantGenesisMinion Cards.mgDelgado (Sch 1) (Atk 2) (HP 6)

mgUnuscione :: MinionCard MutantGenesisMinion
mgUnuscione = minion mkMutantGenesisMinion Cards.mgUnuscione (Sch 2) (Atk 2) (HP 4)

mgNimrod :: MinionCard MutantGenesisMinion
mgNimrod = minion mkMutantGenesisMinion Cards.mgNimrod (Sch 2) (Atk 3) (HP 9)

mgBastion :: MinionCard MutantGenesisMinion
mgBastion = minion mkMutantGenesisMinion Cards.mgBastion (Sch 2) (Atk 2) (HP 10)

instance HasAbilities MutantGenesisMinion where
  getAbilities (MutantGenesisMinion (a `With` _)) =
    [windowAbility a 1 (W.EnemyAttacked W.After enemy AnyIdentity) ForcedResponse NoCost (runAbility a 1) | code `elem` ["32073", "32075"]]
    <> [windowAbility a 2 (W.EnemyAttackedAndDamaged enemy AnyCharacter) ForcedResponse NoCost (runAbility a 2) | code `elem` ["32074", "32076"]]
    <> [windowAbility a 3 (W.EngagesMinion W.After AnyIdentity $ MinionWithId mid) ForcedResponse NoCost (runAbility a 3) | code == "32114"]
   where
    mid = minionId a
    enemy = EnemyWithId $ EnemyMinionId mid
    code = toCardCode a

instance HasModifiersFor MutantGenesisMinion where
  getModifiersFor _ target (MutantGenesisMinion (a `With` _))
    | toCardCode a == "32080", VillainRef _ <- target = pure [CannotBeAttacked]
    | toCardCode a == "32080", target == toRef a = do
        villains <- maybeToList <$> selectOne ActiveVillain
        case villains of
          v : _ -> do
            atk <- getVillainAttackValue v
            sch <- getVillainSchemeValue v
            pure [AttackModifier atk, SchemeModifier sch]
          [] -> pure []
    | toCardCode a == "32161", target == toRef a = pure [KeywordModifier Piercing]
    | toCardCode a == "32166", target == toRef a = pure [MaximumDamagePerPhase 3]
  getModifiersFor _ _ _ = pure []

-- Defeat text is also addressed explicitly by Zeal for the Cause, without defeating
-- or discarding the minion. Each action uses the minion's current engaged player.
acolyteDefeat :: (ReverseQueue m) => Attrs Minion -> IdentityId -> m ()
acolyteDefeat a you = do
  case toCardCode a of
    "32159" -> discardUntilIntoPlay you (CardWithTrait Trait.Acolyte <> CardWithType MinionType)
    "32160" -> do
      confused <- selectAny (IdentityWithId you <> ConfusedIdentity)
      if confused then placeThreatOnEach a 2 EncounterMainScheme else giveStatus you ConfusedStatus
    "32161" -> do
      stunned <- selectAny (IdentityWithId you <> StunnedIdentity)
      if stunned then damageIdentity you a (toDamage 3 FromAbility) else giveStatus you StunnedStatus
    "32162" -> withTheVillain you $ \v -> do
        removeStatus v StunnedStatus
        removeStatus v StunnedStatus
        removeStatus v ConfusedStatus
        removeStatus v ConfusedStatus
        dealBoost v
    "32163" -> withTheVillain you $ \v -> do
        tough <- selectAny (CharacterWithId (toCharacter v) <> CharacterWithStatus ToughStatus)
        if tough then heal v 4 else giveStatus v ToughStatus
    _ -> pure ()

instance RunMessage MutantGenesisMinion where
  runMessage msg e@(MutantGenesisMinion (attrs `With` (defeater, pyroState))) = runQueueT $ case msg of
    WithDiscarded (isTarget attrs -> True) FromDeck cards | Just (you, total, remaining) <- pyroState -> do
      let icons = fromIntegral $ sum $ map (length . cdResources . getCardDef) cards
          sumIcons = total + icons
      if remaining > 1
        then do
          discardFromDeck you 1 attrs
          pure $ MutantGenesisMinion (attrs `With` (defeater, Just (you, sumIcons, remaining - 1)))
        else do
          dealIndirectDamage you attrs sumIcons
          pure $ MutantGenesisMinion (attrs `With` (defeater, Nothing))
    CardSpecific (isTarget attrs -> True) "pyroStart" (fromJSON -> Success you) -> do
      discardFromDeck you 1 attrs
      pure $ MutantGenesisMinion (attrs `With` (defeater, Just (you, 0, 2)))
    MinionMessage mid (MinionDefeatedWithExcess source _ _) | mid == minionId attrs -> do
      player <- getSourceController source
      a <- runMessage msg attrs
      pure $ MutantGenesisMinion (a `With` (player, pyroState))

    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      case toCardCode attrs of
        "32101" -> do
          present <- selectAny (SideSchemeIs Sides.mgOperationZeroTolerance)
          if present then surge you else searchAndRevealEncounterCard you (CardIs Sides.mgOperationZeroTolerance)
        "32102" -> do
          cards <- filter (cardMatch $ CardIs Attachments.mgEnergyBarrier) <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
          for_ (headMay cards) (\card -> do
            cardSpecific GameRef "mgAttachTopSentinel" (mid, card)
            giveStatus mid ToughStatus)
          shuffleEncounterDeck
        "32105" -> do
          marked <- any (== Just you) <$> (selectList (AttachmentIs Attachments.mgTargetedForElimination) >>= traverse (getAttachmentIdentity))
          if marked then enemyAttacks mid you else searchAndRevealEncounterCard you (CardIs Attachments.mgTargetedForElimination)
        _ -> pure ()
      MutantGenesisMinion . (`With` (defeater, pyroState)) <$> runMessage msg attrs
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      you <- maybe getYou pure defeater
      if toCardCode attrs == "32159"
        then discardUntil FromEncounterDeck (CardWithTrait Trait.Acolyte <> CardWithType MinionType) attrs
        else acolyteDefeat attrs you
      when (toCardCode attrs == "32146") $ villainNamed "Magneto" >>= traverse_ (\v -> giveStatus v ToughStatus)
      MutantGenesisMinion . (`With` (defeater, pyroState)) <$> runMessage msg attrs
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) | toCardCode attrs == "32159" -> do
      you <- maybe getYou pure defeater
      afterMatching (\case
        DefeatedLeavesPlay target -> target == toTarget attrs
        ResolveMinionLeavingPlay mid _ -> mid == minionId attrs
        _ -> False) (putEncounterCardIntoPlay you card)
      pure e
    CardSpecific (isTarget attrs -> True) "mgResolveDefeat" _ -> e <$ acolyteDefeat attrs (minionEngagedIdentity attrs)
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      if toCardCode attrs == "32073"
        then do
          characters <- readyCharacters you
          chooseOrNothing you [targeting c (exhaustFriendly c) | c <- characters]
        else do
          cardSpecific attrs "pyroStart" you
      pure e
    RanAbility _ (isTarget attrs -> True) 2 [W.EnemyAttacksAndDamages _ character _] _ -> do
      if toCardCode attrs == "32074" then giveStatus character StunnedStatus else do
        controller <- getSourceController (toSource $ characterRef character)
        for_ controller (\p -> discardFrom p RandomFromHand 1)
      pure e
    RanAbility _ (isTarget attrs -> True) 3 _ _ -> do
      discard <- getEncounterDiscard
      for_ (find (cardMatch (CardWithTrait Trait.Sentinel <> CardWithType AttachmentType)) discard) \card -> do
        cardSpecific GameRef "mgAttachTopSentinel" (minionId attrs, card)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "32073" -> do
          characters <- readyCharacters you
          chooseOrNothing you [targeting c (exhaustFriendly c) | c <- characters]
        "32076" -> discardFrom you RandomFromHand 1
        "32093" -> putBoostIntoPlay attrs you
        "32102" -> do
          stunned <- selectAny (IdentityWithId you <> StunnedIdentity)
          if stunned then damageIdentity you attrs (toDamage 2 FromAbility) else giveStatus you StunnedStatus
        "32106" -> do
          marked <- selectList (AttachmentIs Attachments.mgTargetedForElimination)
          unless (null marked) $ cardSpecific GameRef "mgDealMarkedBoost" (toRef attrs, you)
        "32146" -> do
          villainNamed "Magneto" >>= traverse_ (\v -> giveStatus v ToughStatus >> dealBoost v)
        "32167" -> dealBoostAsEncounterCard attrs you
        _ -> pure ()
      pure e
    _ -> MutantGenesisMinion . (`With` (defeater, pyroState)) <$> runMessage msg attrs

chooseOrNothing :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrNothing you choices = unless (null choices) $ chooseOrRunOne you choices

exhaustFriendly :: (ReverseQueue m) => CharacterId -> m ()
exhaustFriendly = \case
  IdentityCharacter you -> exhaust you
  AllyCharacter ally -> exhaust ally
  _ -> pure ()

readyCharacters :: (HasGame m) => IdentityId -> m [CharacterId]
readyCharacters you = do
  identities <- selectList (IdentityWithId you <> UnexhaustedIdentity)
  allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
  pure $ map IdentityCharacter identities <> map AllyCharacter allies
