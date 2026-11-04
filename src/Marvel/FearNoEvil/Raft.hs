module Marvel.FearNoEvil.Raft where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Identity.Types (Field(PlayerIdentityDeck))
import Marvel.Keyword
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype RaftBreakout = RaftBreakout (Attrs MainScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
raftBreakout :: MainSchemeCard RaftBreakout
raftBreakout = mainScheme RaftBreakout Cards.street60142b (PerPlayer 11) (PerPlayer 2) (PerPlayer 1)
instance IsMainScheme RaftBreakout where
  mainSchemeAbilities = getAbilities
  mainSchemeModifiersFor _ _ _ = pure []
instance HasAbilities RaftBreakout where getAbilities _ = []
instance RunMessage RaftBreakout where
  runMessage msg e@(RaftBreakout a) = runQueueT $ case msg of
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      first <- selectJust FirstPlayer
      key <- findEncounterCards $ CardIs Cards.street60143
      for_ (headMay key) $ putEncounterCardIntoPlay first
      players <- getAllPlayers
      for_ players $ \you -> discardUntilAndReveal you (CardWithType MinionType <> CardWithTrait T.Prisoner)
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> gameOver Lost >> pure e
    _ -> RaftBreakout <$> runMessage msg a

newtype MasterKey = MasterKey (Attrs Attachment `With` [(IdentityId,EncounterCard)])
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasModifiersFor
instance IsAttachment MasterKey where toAttachmentAttrs (MasterKey (a `With` _)) = a
masterKey :: AttachmentCard MasterKey
masterKey = attachment (\a -> MasterKey $ a `With` []) Cards.street60143
instance HasAbilities MasterKey where
  getAbilities (MasterKey (a `With` _)) = [windowAbility a 1 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 1) | Just (EnemyVillainId vid) <- [attachmentEnemy a]]
instance RunMessage MasterKey where
  runMessage msg e@(MasterKey (a `With` cards)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> withTheVillain you (attachTo aid) >> pure e
    BoostCardResolved enemy card | Just enemy == attachmentEnemy a,cdCardType (getCardDef card) == MinionType -> do
      you <- getBoostedPlayer enemy
      pure $ MasterKey (a `With` (cards <> [(you,card)]))
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ cards $ \(you,card) -> whenM (isCurrentBoostCard card) $ consumeBoostCard card >> dealFoundEncounterCard you card
      pure $ MasterKey (a `With` [])
    VillainMessage vid VillainEndAttack | Just (EnemyVillainId vid) == attachmentEnemy a -> pure $ MasterKey (a `With` [])
    _ -> (\attrs -> MasterKey $ attrs `With` cards) <$> runMessage msg a

newtype Prisoner = Prisoner (Attrs Minion `With` [(IdentityId,EncounterCard)])
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
instance IsMinion Prisoner where
  toMinionAttrs (Prisoner (a `With` _)) = a
  overMinionAttrs f (Prisoner (a `With` cards)) = Prisoner (f a `With` cards)
  minionDisplayCounters (Prisoner (a `With` _)) = minionCounters a
prisoners :: [MinionCard Prisoner]
prisoners = [minion (\a -> Prisoner $ a `With` []) def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp) <-
  [(Cards.street60144,1,1,6),(Cards.street60145,2,1,6),(Cards.street60146,1,1,6),(Cards.street60147,2,1,6),(Cards.street60148,1,2,7),(Cards.street60149,1,3,7)]]
instance HasModifiersFor Prisoner where
  getModifiersFor _ target (Prisoner (a `With` _)) = pure [KeywordModifier Overkill | target == toRef a,toCardCode a == "60149"]
instance HasAbilities Prisoner where
  getAbilities (Prisoner (a `With` _)) = case toCardCode a of
    "60144" -> [windowAbility a 1 attack ForcedInterrupt NoCost (runAbility a 1)]
    "60146" -> [windowAbility a 1 attacked ForcedResponse NoCost (runAbility a 1)]
    "60147" -> [windowAbility a 1 attacked ForcedResponse NoCost (runAbility a 1)]
    "60148" -> [windowAbility a 1 attack ForcedInterrupt NoCost (runAbility a 1)]
    _ -> []
   where
    attack = W.EnemyAttacked W.When (EnemyWithId $ toEnemyId a) AnyIdentity
    attacked = W.EnemyAttacked W.After (EnemyWithId $ toEnemyId a) AnyIdentity
instance RunMessage Prisoner where
  runMessage msg e@(Prisoner (a `With` boosts)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a,toCardCode a == "60145" -> do
      allies <- selectList $ AllyControlledBy AnyIdentity
      eligible <- filterM (fmap (== 0) . getCharacterThreat . AllyCharacter) allies
      unless (null eligible) $ chooseOne you [targeting (AllyRef ally) (placeCharacterThreat ally 1) | ally <- eligible]
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      you <- getBoostedPlayer $ toEnemyId a
      case toCardCode a of
        "60144" -> do
          deck <- unDeck <$> project PlayerIdentityDeck you
          for_ (headMay deck) $ tuckPlayerCardUnder a
          cardSpecific a "absorbedResources" ()
        "60146" -> placeCounters a 1 >> dealIndirectDamage you a (minionCounters a + 1)
        "60147" -> for_ boosts $ \(owner,card) -> whenM (isCurrentBoostCard card) $ consumeBoostCard card >> dealFoundEncounterCard owner card
        "60148" -> chooseOne you
          [labeled "$label.fnePlaceThreat" $ placeThreatOnEach a 1 EncounterMainScheme
          ,labeled "$label.fneProximaAttackBonus" $ untilEnemyActivationEnds a (minionId a) [AttackModifier 2]]
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "absorbedResources" _ -> do
      held <- getZoneHeldCards (toRef a)
      let resources = nub $ concatMap (map snd . cdResources . getCardDef) held
      untilEnemyActivationEnds a (minionId a) [AttackModifier $ fromIntegral $ length resources]
      pure e
    BoostCardResolved enemy card | enemy == toEnemyId a,toCardCode a == "60147",cdCardType (getCardDef card) == TreacheryType -> do
      you <- getBoostedPlayer enemy
      pure $ Prisoner (a `With` (boosts <> [(you,card)]))
    MinionMessage mid MinionEndAttack | mid == minionId a -> do
      a' <- runMessage msg a
      pure $ Prisoner (a' `With` [])
    Boost (RevealedAsBoost target enemy) | isTarget a target,toCardCode a == "60149" -> giveStatus (toCharacter enemy) ToughStatus >> pure e
    _ -> (\attrs -> Prisoner $ attrs `With` boosts) <$> runMessage msg a

newtype Imprisoned = Imprisoned (Attrs Obligation)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsObligation
imprisoned :: ObligationCard Imprisoned
imprisoned = obligation Imprisoned Cards.street60150
instance HasModifiersFor Imprisoned where
  getModifiersFor _ target (Imprisoned a) = pure [modifier | Just target == (IdentityRef <$> obligationOwner a),modifier <- [CannotThwart,CannotAttack,CannotChangeToAlterEgoForm]]
instance HasAbilities Imprisoned where
  getAbilities (Imprisoned a) = [ability a index HeroAction (SelfMatches $ IdentityWithId you) cost (runAbility a index) | Just you <- [obligationOwner a],(index,cost) <- [(1,MultiResourceCost $ replicate 3 Nothing),(2,ExhaustCharactersWithTotalThwartCost 3)]]
instance RunMessage Imprisoned where
  runMessage msg e@(Imprisoned a) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> pure $ Imprisoned a {obligationOwner=Just you}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility _ (isTarget a -> True) index _ _ | index `elem` [1,2] -> removeFromPlay a >> pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCard a of EncounterCard card -> consumeBoostCard card >> revealFoundEncounterCard you card; _ -> pure ()
      pure e
    _ -> Imprisoned <$> runMessage msg a
