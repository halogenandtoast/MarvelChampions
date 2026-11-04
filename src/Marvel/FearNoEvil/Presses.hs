module Marvel.FearNoEvil.Presses where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Entity
import Marvel.Environment.Types
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment (paymentExhaustedCards)
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Support.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types
import Marvel.Window qualified as W
import System.Random.Shuffle (shuffleM)

newtype StopThePresses = StopThePresses (Attrs MainScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
stopThePresses :: MainSchemeCard StopThePresses
stopThePresses = mainScheme StopThePresses Cards.street60151b (PerPlayer 9) (PerPlayer 1) (PerPlayer 1)
instance IsMainScheme StopThePresses where
  mainSchemeAbilities = getAbilities
  mainSchemeModifiersFor _ _ _ = pure []
instance HasAbilities StopThePresses where
  getAbilities (StopThePresses a) = [windowAbility a 1 (W.EnemyAttacked W.After UndefendedEnemy AnyIdentity) ForcedResponse NoCost (runAbility a 1)]
instance RunMessage StopThePresses where
  runMessage msg e@(StopThePresses a) = runQueueT $ case msg of
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      players <- getAllPlayers
      first <- selectJust FirstPlayer
      environments <- findEncounterCards $ CardIs Cards.street60152
      for_ (headMay environments) $ putEncounterCardIntoPlay first
      cards <- shuffleM =<< traverse genEncounterCard [Cards.street60153,Cards.street60154,Cards.street60155,Cards.street60156]
      setAside $ map EncounterCard cards
      for_ (zip players cards) $ \(you,card) -> putEncounterCardIntoPlay you card
      traverse_ removeFromGame $ drop (length players) cards
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let attacked = [ident | W.EnemyAttack _ ident <- windows]
      for_ attacked $ \owner -> do
        supports <- selectList $ SupportWithTrait T.DailyBugle <> SupportControlledBy (IdentityWithId owner) <> SupportWithUses (GreaterThan $ Static 0)
        expert <- (== Expert) <$> getDifficulty
        chooseOne you $ [labeled "$label.fnePlaceThreat" $ placeThreatOn a (mainSchemeId a) (if expert then 3 else 2)]
          <> [targeting sid (removeCounters sid 1) | sid <- supports]
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> gameOver Lost >> pure e
    _ -> StopThePresses <$> runMessage msg a

newtype DailyBugle = DailyBugle (Attrs Environment)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsEnvironment,HasModifiersFor)
dailyBugle :: EnvironmentCard DailyBugle
dailyBugle = environment DailyBugle Cards.street60152
instance HasAbilities DailyBugle where
  getAbilities (DailyBugle a) = [ability a 1 AlterEgoAction NoCriteria (ExhaustSupportCost $ SupportWithTrait T.DailyBugle) $ runAbility a 1]
instance RunMessage DailyBugle where
  runMessage msg e@(DailyBugle a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ payment -> do
      for_ [sid | SupportRef sid <- paymentExhaustedCards payment] $ \sid -> addSupportUses sid 1
      pure e
    _ -> DailyBugle <$> runMessage msg a

newtype BugleStaff = BugleStaff (Attrs Support)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsSupport,HasModifiersFor)
bugleStaff :: [SupportCard BugleStaff]
bugleStaff = [supportWith BugleStaff def ((usesL .~ 3) . (discardIfNoUsesL .~ True)) | def <- [Cards.street60153,Cards.street60154,Cards.street60155,Cards.street60156]]
instance HasAbilities BugleStaff where
  getAbilities (BugleStaff a) = case toCardCode a of
    "60153" -> [ability a 1 Action OwnsThis cost $ runAbility a 1]
    "60154" -> [windowAbility a 1 (W.BoostCardFlipped W.When AnyEnemy) Interrupt cost $ runAbility a 1]
    "60155" -> [ability a 1 Action OwnsThis (Costs [cost,DealEncounterCardCost]) $ runAbility a 1]
    _ -> [windowAbility a 1 (W.EncounterCardDealtTo AnyIdentity) Response cost $ runAbility a 1]
   where cost = Costs [ExhaustCost,UseCost]
instance RunMessage BugleStaff where
  runMessage msg e@(BugleStaff a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "60153" -> lookAtTopOfEncounterDeck 2 a
        "60154" -> for_ [(card,enemy) | W.FlippedBoostCard card enemy <- windows] $ \(card,enemy) -> do
          cancelBoostEffect card
          cancelBoostIconsOnCard card
          dealBoost enemy
        "60155" -> drawCards you 2
        _ -> for_ [(owner,card) | W.EncounterCardWasDealt owner card <- windows] $ \(owner,card) -> focusing [EncounterCard card] $
          chooseOne you [labeled "$label.fneKeepEncounterCard" (pure ()),labeled "$label.fneReplaceEncounterCard" $ replaceDealtEncounterCard owner card]
      pure e
    SearchFoundCards (isTarget a -> True) cards -> do
      let you = supportController a
      focusing cards $ chooseOne you $ [labeled "$label.done" (pure ())]
        <> [targeting card (discardKnownEncounterCardFromDeck you card) | EncounterCard card <- cards]
      pure e
    _ -> BugleStaff <$> runMessage msg a

newtype ExclusiveInterview = ExclusiveInterview (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasAbilities
instance IsSideScheme ExclusiveInterview where sideSchemeAbilities = getAbilities
exclusiveInterview :: SideSchemeCard ExclusiveInterview
exclusiveInterview = sideScheme ExclusiveInterview Cards.street60157 (Static 6)
instance HasModifiersFor ExclusiveInterview where
  getModifiersFor _ (SupportRef sid) _ = do
    bugle <- maybe False (elem T.DailyBugle . cdTraits) <$> getTargetCardDef (SupportRef sid)
    pure [CannotReady | bugle]
  getModifiersFor _ _ _ = pure []
instance RunMessage ExclusiveInterview where
  runMessage msg e@(ExclusiveInterview a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> selectList (SupportWithTrait T.DailyBugle) >>= traverse_ exhaust >> pure e
    _ -> ExclusiveInterview <$> runMessage msg a

newtype BreakingNews = BreakingNews (Attrs Treachery)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsTreachery,HasAbilities,HasModifiersFor)
breakingNews :: TreacheryCard BreakingNews
breakingNews = treachery BreakingNews Cards.street60158
instance RunMessage BreakingNews where
  runMessage msg e@(BreakingNews a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      supports <- selectList $ SupportWithTrait T.DailyBugle <> UnexhaustedSupport <> SupportWithUses (GreaterThan $ Static 0)
      chooseOne you $ [targeting sid (exhaust sid >> removeCounters sid 1) | sid <- supports]
        <> [labeled "$label.fnePlaceThreat" $ placeThreatOnEach a 2 EncounterMainScheme]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      supports <- selectList $ SupportWithTrait T.DailyBugle <> UnexhaustedSupport
      unless (null supports) $ chooseOne you [targeting sid (exhaust sid) | sid <- supports]
      pure e
    _ -> BreakingNews <$> runMessage msg a
