module Marvel.FearNoEvil.Getaway where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Window qualified as W

speed :: HasGame m => m Natural
speed = getCountersOnMainSchemes

newtype Getaway = Getaway (Attrs MainScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
getaway :: MainSchemeCard Getaway
getaway = mainScheme Getaway Cards.street60128b (PerPlayer 9) (PerPlayer 2) (PerPlayer 2)
instance IsMainScheme Getaway where
  mainSchemeAbilities = getAbilities
  mainSchemeModifiersFor _ _ _ = pure []
instance HasAbilities Getaway where
  getAbilities (Getaway a) = [windowAbility a 1 W.RoundEnds ForcedInterrupt NoCost (runAbility a 1) | mainSchemeThreat a > 0]
instance RunMessage Getaway where
  runMessage msg e@(Getaway a) = runQueueT $ case msg of
    SetupMainScheme -> do
      expert <- (== Expert) <$> getDifficulty
      placeCounters a $ if expert then 2 else 1
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      you <- selectJust FirstPlayer
      genEncounterCard Cards.street60129a >>= putEncounterCardIntoPlay you
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      placeCounters a 1
      alongside <- selectList $ AttachmentIs Cards.street60129b
      if null alongside then placeCounters a 1 else traverse_ (\aid -> cardSpecific aid "getawayFlip" False) alongside
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> gameOver Lost >> pure e
    _ -> do
      a' <- runMessage msg a
      when (mainSchemeThreat a > 0 && mainSchemeThreat a' == 0) $ do
        attachments <- selectList $ AttachmentIs Cards.street60129a
        traverse_ (\aid -> cardSpecific aid "getawayFlip" True) attachments
      pure $ Getaway a'

newtype ChasePosition = ChasePosition (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsAttachment
chasePositions :: [AttachmentCard ChasePosition]
chasePositions = map (attachment ChasePosition) [Cards.street60129a,Cards.street60129b]
instance HasModifiersFor ChasePosition where
  getModifiersFor _ target (ChasePosition a) = pure [KeywordModifier Ranged | toCardCode a == "60129a",Just target == (enemyRef <$> attachmentEnemy a)]
instance HasAbilities ChasePosition where
  getAbilities (ChasePosition a) = [windowAbility a 1 (W.EnemyWouldBeDealtDamage $ EnemyWithId enemy) ForcedInterrupt NoCost (runAbility a 1) | toCardCode a == "60129a",Just enemy <- [attachmentEnemy a]]
instance RunMessage ChasePosition where
  runMessage msg e@(ChasePosition a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      withTheVillain you (attachTo aid)
      if toCardCode a == "60129b" then cardSpecific a "chaseCollision" you else cardSpecific a "checkOutFront" ()
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [ (enemy,d) | W.EnemyDamageWouldBeDealt enemy _ d <- windows,Just enemy == attachmentEnemy a] $ \(enemy,damage) -> do
        cancelEnemyDamage enemy
        scheme <- selectOne EncounterMainScheme
        -- This encounter attachment removes threat; crisis does not restrict it.
        for_ scheme $ \sid -> removeSchemeThreat a sid (damageAmount damage)
      pure e
    CardSpecific (isTarget a -> True) "checkOutFront" _ | toCardCode a == "60129a" -> do
      main <- selectOne EncounterMainScheme
      for_ main $ \scheme -> do
        threat <- getSchemeThreatAmount scheme
        when (threat == 0) $ cardSpecific a "getawayFlip" True
      pure e
    CardSpecific (isTarget a -> True) "getawayFlip" (fromJSON -> Success alongside) -> do
      when (alongside && toCardCode a == "60129a") $ do
        you <- selectJust FirstPlayer
        cardSpecific a "chaseCollision" you
      unless alongside $ cardSpecific a "checkOutFront" ()
      pure $ ChasePosition a {attachmentCardDef=if alongside then Cards.street60129b else Cards.street60129a}
    CardSpecific (isTarget a -> True) "chaseCollision" (fromJSON -> Success (_ :: IdentityId)) -> do
      getAllPlayers >>= traverse_ (\you -> dealIndirectDamage you a 1)
      n <- fromIntegral <$> fromGameValue (PerPlayer 2)
      for_ (attachmentEnemy a) $ \enemy -> dealDamage enemy a $ toDamage n FromAbility
      pure e
    _ -> ChasePosition <$> runMessage msg a

newtype EscortVehicle = EscortVehicle (Attrs Minion)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsMinion,HasAbilities)
escortVehicle :: MinionCard EscortVehicle
escortVehicle = minion EscortVehicle Cards.street60130 (Sch 2) (Atk 2) (HP 5)
instance HasModifiersFor EscortVehicle where
  getModifiersFor _ target (EscortVehicle a) | target == toRef a = do
    n <- speed
    pure [KeywordModifier keyword | (minimumSpeed,keyword) <- [(3,Toughness),(6,Quickstrike),(9,Surge)],n >= minimumSpeed]
  getModifiersFor _ _ _ = pure []
instance RunMessage EscortVehicle where runMessage msg (EscortVehicle a) = EscortVehicle <$> runMessage msg a

newtype TankerTruck = TankerTruck (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasAbilities
instance IsSideScheme TankerTruck where sideSchemeAbilities = getAbilities
tankerTruck :: SideSchemeCard TankerTruck
tankerTruck = sideScheme TankerTruck Cards.street60131 (Static 2)
instance HasModifiersFor TankerTruck where
  getModifiersFor _ (VillainRef _) _ = pure [CannotBeDealtDamage]
  getModifiersFor _ _ _ = pure []
instance RunMessage TankerTruck where runMessage msg (TankerTruck a) = TankerTruck <$> runMessage msg a

newtype GetawayTreachery = GetawayTreachery (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsTreachery,HasAbilities,HasModifiersFor)
getawayTreacheries :: [TreacheryCard GetawayTreachery]
getawayTreacheries = map (treachery GetawayTreachery) [Cards.street60132,Cards.street60133]
instance RunMessage GetawayTreachery where
  runMessage msg e@(GetawayTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> speed >>= \n -> cardSpecific a "speedChoices" (you,n) >> pure e
    CardSpecific (isTarget a -> True) "speedChoices" (fromJSON -> Success (you,n :: Natural)) -> do
      when (n > 0) $ do
        let next = cardSpecific a "speedChoices" (you,n-1)
        if toCardCode a == "60132" then do
          let cost = ResourceCost Nothing
          payable <- canAffordCost you (toSource a) cost
          chooseOne you $ [labeled "$label.fneSpendResource" $ payCostThenWithPayment you a cost a "speedPaid" (you,n-1) | payable]
            <> [labeled "$label.fnePlaceThreat" $ placeThreatOnEach a 1 EncounterMainScheme >> next]
        else do
          ready <- selectList $ CharacterControlledBy (IdentityWithId you) <> CharacterOneOf [IdentityCharacters UnexhaustedIdentity,AllyCharacters UnexhaustedAlly]
          chooseOne you $ [targeting (characterRef character) ((case character of IdentityCharacter ident -> exhaust ident; AllyCharacter ally -> exhaust ally; _ -> pure ()) >> next) | character <- ready]
            <> [labeled "$label.fneTakeIndirectDamage" $ dealIndirectDamage you a 1 >> next]
      pure e
    CardSpecific (isTarget a -> True) "speedPaid" (fromJSON -> Success ((you,n :: Natural),_ :: Payment)) -> cardSpecific a "speedChoices" (you,n) >> pure e
    _ -> GetawayTreachery <$> runMessage msg a
