module Marvel.FearNoEvil.ArtMuseum where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
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
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Treachery.Types
import Marvel.Window qualified as W
import System.Random.Shuffle (shuffleM)

newtype ArtPiece = ArtPiece (Attrs Attachment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass IsAttachment
artPieces :: [AttachmentCard ArtPiece]
artPieces = map (attachment ArtPiece) [Cards.street60122,Cards.street60123,Cards.street60124,Cards.street60125]
artHost :: Attrs Attachment -> Maybe CharacterId
artHost a = (toCharacter <$> attachmentEnemy a) <|> (IdentityCharacter <$> attachmentIdentity a) <|> (AllyCharacter <$> attachmentAlly a)
instance HasModifiersFor ArtPiece where
  getModifiersFor _ target (ArtPiece a) = pure $ if Just target == (characterRef <$> artHost a) then
    [case target of IdentityRef _ -> ThwartModifier 1; AllyRef _ -> ThwartModifier 1; _ -> SchemeModifier 1]
    <> [KeywordModifier Stalwart | toCardCode a == "60123"] else []
instance HasAbilities ArtPiece where
  getAbilities (ArtPiece a) =
    [label text $ ability a index HeroAction NoCriteria cost $ runAbility a index
    | isNothing $ attachmentIdentity a
    , (index,text,cost) <- [(1,"$label.fnePayForArt",PrintedResourceCost [resource]),(3,"$label.fneExhaustForArt",ExhaustCost)]]
    <> [windowAbility a 2 (W.AttachmentAttachedToCharacter W.After (AttachmentWithId $ attachmentId a) AnyCharacter) ForcedResponse NoCost (runAbility a 2) | toCardCode a /= "60123"]
   where
    resource = case toCardCode a of "60122" -> Mental; "60123" -> Wild; "60124" -> Physical; _ -> Energy
instance RunMessage ArtPiece where
  runMessage msg e@(ArtPiece a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> withTheVillain you (attachTo aid) >> pure e
    RanAbility you (isTarget a -> True) index _ _ | index `elem` [1,3] -> attachToIdentity (attachmentId a) you >> pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      for_ (artHost a) $ \host -> case toCardCode a of
        "60122" -> giveStatus host ConfusedStatus
        "60124" -> giveStatus host ToughStatus
        "60125" -> giveStatus host StunnedStatus
        _ -> pure ()
      pure e
    _ -> ArtPiece <$> runMessage msg a

-- Art moves with its printed after-attachment effects; it is never revealed again.
moveArtToVillain :: ReverseQueue m => IdentityId -> [AttachmentId] -> m ()
moveArtToVillain you cards = unless (null cards) $ withTheVillain you $ \vid -> chooseOne you [targeting (AttachmentRef aid) (attachTo aid vid) | aid <- cards]
artOnIdentities :: HasGame m => m [AttachmentId]
artOnIdentities = selectList (AttachmentWithTrait T.Art) >>= filterM (fmap (\case Just (IdentityCharacter _) -> True; _ -> False) . getAttachedCharacter)

newtype MuseumHeist = MuseumHeist (Attrs MainScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
museumHeist :: MainSchemeCard MuseumHeist
museumHeist = mainScheme MuseumHeist Cards.street60121b (PerPlayer 9) (PerPlayer 1) (PerPlayer 1)
instance IsMainScheme MuseumHeist where
  mainSchemeAbilities = getAbilities
  mainSchemeModifiersFor _ _ _ = pure []
instance HasAbilities MuseumHeist where
  getAbilities (MuseumHeist a) = [windowAbility a 1 (W.EnemyAttacked W.After (UndefendedEnemy <> EnemyAttackingCharacter (CharacterWithAttachment $ AttachmentWithTrait T.Art)) AnyIdentity) ForcedResponse NoCost (runAbility a 1)]
instance RunMessage MuseumHeist where
  runMessage msg e@(MuseumHeist a) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      cards <- findEncounterCards $ CardWithTrait T.Art
      shuffled <- shuffleM cards
      for_ (headMay shuffled) $ \card -> withTheVillain first $ \vid -> attachEncounterToCharacter card (VillainCharacter vid)
      pure e
    AccelerateMainScheme -> do
      arts <- selectList $ AttachmentWithTrait T.Art <> AttachmentOnVillain AnyVillain
      MuseumHeist <$> runMessage msg a {mainSchemeAcceleration=PerPlayer $ 1 + length arts}
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      for_ [ident | W.EnemyAttack _ ident <- windows] $ \ident -> do
        cards <- selectList (AttachmentWithTrait T.Art) >>= filterM (fmap (== Just (IdentityCharacter ident)) . getAttachedCharacter)
        moveArtToVillain you cards
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> gameOver Lost >> pure e
    _ -> MuseumHeist <$> runMessage msg a

newtype ArtThief = ArtThief (Attrs Minion)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsMinion,HasAbilities,HasModifiersFor)
artThief :: MinionCard ArtThief
artThief = minion ArtThief Cards.street60126 (Sch 2) (Atk 2) (HP 3)
instance RunMessage ArtThief where
  runMessage msg e@(ArtThief a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      candidates <- filter (cardMatch $ CardWithTrait T.Art) <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
      if null candidates then artOnIdentities >>= moveArtToVillain you else withTheVillain you $ \vid ->
        chooseOne you [cardLabeled (toCardCode card) (attachEncounterToCharacter card $ VillainCharacter vid) | card <- candidates]
      shuffleEncounterDeck
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      artOnIdentities >>= moveArtToVillain you
      pure e
    _ -> ArtThief <$> runMessage msg a

newtype TugOfWar = TugOfWar (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsTreachery,HasAbilities,HasModifiersFor)
tugOfWar :: TreacheryCard TugOfWar
tugOfWar = treachery TugOfWar Cards.street60127
instance RunMessage TugOfWar where
  runMessage msg e@(TugOfWar a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      cards <- selectList $ AttachmentWithTrait T.Art <> AttachmentOnVillain AnyVillain
      let cost = PrintedResourceCost [Physical,Physical,Physical]
      payable <- canAffordCost you (toSource a) cost
      chooseOne you $ [labeled "$label.fneTakeArt" $ payCostThenWithPayment you a cost a "tugArt" you | payable,not (null cards)]
        <> [labeled "$label.fneLoseArt" $ artOnIdentities >>= moveArtToVillain you >> gainSurge a]
      pure e
    CardSpecific (isTarget a -> True) "tugArt" (fromJSON -> Success (you,_ :: Payment)) -> do
      cards <- selectList $ AttachmentWithTrait T.Art <> AttachmentOnVillain AnyVillain
      unless (null cards) $ chooseOne you [targeting (AttachmentRef aid) (attachToIdentity aid you) | aid <- cards]
      pure e
    _ -> TugOfWar <$> runMessage msg a
