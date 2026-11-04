module Marvel.FearNoEvil.CampaignCards where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Types qualified as A
import Marvel.Ally.Runner ()
import Marvel.Campaign qualified as C
import Marvel.Card
import Marvel.Cost
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.Environment.Types
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Name (title)
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import System.Random.Shuffle (shuffleM)

newtype CampaignEnvironment = CampaignEnvironment (Attrs Environment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsEnvironment,HasAbilities)
campaignEnvironments :: [EnvironmentCard CampaignEnvironment]
campaignEnvironments = map (environment CampaignEnvironment) [Cards.street60205a,Cards.street60205b,Cards.street60206a,Cards.street60206b,Cards.street60207a,Cards.street60207b,Cards.street60208a,Cards.street60208b,Cards.street60209a,Cards.street60209b]
instance HasModifiersFor CampaignEnvironment where
  getModifiersFor _ (AttachmentRef _) (CampaignEnvironment a) | toCardCode a == "60206b" -> do
    expert <- (== Expert) <$> getDifficulty
    roundNumber <- getRound
    pure [CannotBeDiscarded | expert,roundNumber == 1]
  getModifiersFor _ _ _ = pure []
instance RunMessage CampaignEnvironment where
  runMessage msg e@(CampaignEnvironment a) = runQueueT $ case msg of
    CardSpecific (isTarget a -> True) "campaignEnvironmentSetup" _ -> do
      players <- getPlayers
      expert <- (== Expert) <$> getDifficulty
      case toCardCode a of
        "60205a" -> traverse_ (`giveStatus` ToughStatus) players
        "60205b" -> for_ players $ \you -> if expert then stunIdentity you >> confuseIdentity you else chooseOne you
          [labeled "$label.fneBeStunned" $ stunIdentity you,labeled "$label.fneBeConfused" $ confuseIdentity you]
        "60206a" -> offerSearch players UpgradeType
        "60207a" -> offerSearch players SupportType
        "60208a" -> offerSearch players AllyType
        "60206b" -> cardSpecific a "failedGetaway" players
        "60207b" -> for_ players $ \you -> discardFrom you FromHand $ if expert then 2 else 1
        "60208b" -> do
          cards <- shuffleM =<< traverse genEncounterCard [Cards.street60144,Cards.street60145,Cards.street60146,Cards.street60147,Cards.street60148,Cards.street60149]
          setAside $ map EncounterCard cards
          when expert $ for_ (zip players cards) $ \(you,card) -> dealFoundEncounterCard you card
          shuffleIntoEncounterDeck $ if expert then drop (length players) cards else cards
        "60209a" -> cardSpecific a "completedPresses" players
        "60209b" -> for_ players $ \you -> replicateM_ (if expert then 2 else 1) $ dealEncounterCard you
        _ -> pure ()
      pure e
    SearchFoundCards (isTarget a -> True) [PlayerCard card] -> do
      you <- maybe getYou pure (pcOwner card)
      addToHand you card
      whenM ((== Expert) <$> getDifficulty) $ discardFrom you FromHand 1
      pure e
    CardSpecific (isTarget a -> True) "failedGetaway" (fromJSON -> Success (players :: [IdentityId])) -> do
      case players of
        [] -> pure ()
        you:rest -> do
          cards <- filter (cardMatch $ CardWithType AttachmentType) <$> getEncounterDeck
          if null cards then dealEncounterCard you else chooseOne you [cardLabeled (toCardCode card) (revealEncounterCardFrom W.RevealedFromEncounterDeck you card) | card <- cards]
          shuffleEncounterDeck
          cardSpecific a "failedGetaway" rest
      pure e
    CardSpecific (isTarget a -> True) "completedPresses" (fromJSON -> Success (players :: [IdentityId])) -> do
      case players of
        [] -> pure ()
        you:rest -> do
          campaign <- getCampaign
          cards <- filter (\card -> cdCardType (getCardDef card) == SupportType && T.DailyBugle `elem` cdTraits (getCardDef card)) <$> getSetAsideEncounterCards
          supports <- selectList $ SupportWithTrait T.DailyBugle
          inPlay <- traverse (getTargetCardDef . SupportRef) supports
          let removed = maybe [] (C.removedTitles . C.fearNoEvil . C.campaignLog) campaign
              occupied = [title $ cdName def | Just def <- inPlay]
              available = [card | card <- cards,title (cdName $ getCardDef card) `notElem` removed <> occupied]
          unless (null available) $ chooseOne you [cardLabeled (toCardCode card) (putEncounterCardIntoPlay you card) | card <- available]
          cardSpecific a "completedPresses" rest
      pure e
    _ -> CampaignEnvironment <$> runMessage msg a
   where
    offerSearch players kind = for_ players $ \you -> chooseOne you
      [labeled "$label.fneSearchCampaignReward" $ search you (SearchIdentityDeckAndDiscardPile you) (CardWithType kind) (SearchTarget $ toRef a) ShuffleBackIn
      ,labeled "$label.done" (pure ())]

newtype CampaignMary = CampaignMary (Attrs A.Ally)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (A.IsAlly,HasModifiersFor)
campaignMaryFaces :: [A.AllyCard CampaignMary]
campaignMaryFaces = [A.ally CampaignMary Cards.street60210a (Thw 2,1) (Atk 1,1) (HP 3),A.ally CampaignMary Cards.street60210b (Thw 1,1) (Atk 2,1) (HP 3)]
instance HasAbilities CampaignMary where
  getAbilities (CampaignMary a) = [windowAbility a 1 (W.EncounterCardRevealedBy W.After (IdentityWithId $ A.allyController a) $ EncounterCardMatching $ CardWithType kind) ForcedResponse NoCost (runAbility a 1)]
   where kind = if toCardCode a == "60210a" then MinionType else SideSchemeType
instance RunMessage CampaignMary where
  runMessage msg e@(CampaignMary a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      healFrom a (A.allyId a) (A.allyDamage a)
      flipAlly $ A.allyId a
      pure e
    _ -> CampaignMary <$> runMessage msg a
