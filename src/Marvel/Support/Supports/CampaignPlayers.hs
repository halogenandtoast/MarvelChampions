{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.CampaignPlayers where
import Marvel.Prelude
import Marvel.Name qualified as Name
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.CampaignPlayers.Cards qualified as Cards
import Marvel.CampaignPlayers.Helpers
import Marvel.FalconWinter.Helpers (nextCardReduction)
import Marvel.Support.Types

cw56003 :: SupportCard CampaignSupport
cw56003 = support CampaignSupport Cards.cw56003

cw56012 :: SupportCard CampaignSupport
cw56012 = supportWith CampaignSupport Cards.cw56012 (\a -> a {supportUses = 2, supportDiscardIfNoUses = True})

cw56031 :: SupportCard CampaignSupport
cw56031 = support CampaignSupport Cards.cw56031

cw56043 :: SupportCard CampaignSupport
cw56043 = supportWith CampaignSupport Cards.cw56043 (\a -> a {supportUses = 3, supportDiscardIfNoUses = True})

fne60013 :: SupportCard CampaignSupport
fne60013 = support CampaignSupport Cards.fne60013

fne60014 :: SupportCard CampaignSupport
fne60014 = support CampaignSupport Cards.fne60014

fne60015 :: SupportCard CampaignSupport
fne60015 = support CampaignSupport Cards.fne60015

fne60016 :: SupportCard CampaignSupport
fne60016 = support CampaignSupport Cards.fne60016

fne60029 :: SupportCard CampaignSupport
fne60029 = support CampaignSupport Cards.fne60029

fne60042 :: SupportCard CampaignSupport
fne60042 = support CampaignSupport Cards.fne60042

newtype CampaignSupport = CampaignSupport (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities CampaignSupport where
  getAbilities s@(CampaignSupport attrs) = case Name.title (cdName (supportCardDef attrs)) of
    "Precinct HQ" -> [ability s 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility s 1]
    "Air Cover" -> [limitedWindowAbility s 1 (W.MinionEntersPlay W.After AnyMinion) Response OwnsThis (ExhaustCost <> UseCost) $ runAbility s 1]
    "Altman Residence" -> [ability s 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility s 1]
    "Hidden Base" -> [limitedWindowAbility s 1 (W.IdentityChangedToForm W.After $ IdentityWithId $ supportController attrs) AlterEgoResponse OwnsThis (ExhaustCost <> UseCost) $ runAbility s 1]
    "Foggy Nelson" -> [ability s 1 AlterEgoAction (OwnsThis <> SchemeExists ThwartableScheme) ExhaustCost $ runAbility s 1]
    "Karen Page" -> [ability s 1 Action OwnsThis ExhaustCost $ runAbility s 1]
    "Nelson and Murdock" -> [limitedWindowAbility s 1 (W.SideSchemeDefeatedBy AnySideScheme $ SourceWithTrait T.Attorney) Response (OwnsThis <> EnemyExists EnemyCanBeConfused) NoCost $ runAbility s 1]
    "Sister Maggie" -> [limitedWindowAbility s 1 (W.UsesRecoveryAt W.After $ IdentityWithId $ supportController attrs) Response (OwnsThis <> AnyCriteria [CharacterExists (CharacterWithId (IdentityCharacter $ supportController attrs) <> CharacterWithStatus status) | status <- [minBound .. maxBound]]) NoCost $ runAbility s 1]
    "Stick" -> [label (if n == 1 then "$label.stickExhaust" else "$label.stickReady") $ limited (SharedPerWindow ("stick:" <> tshow (supportId attrs)) 1) $ limitedWindowAbility s n matcher Interrupt OwnsThis cost $ runAbility s n | (n,cost) <- [(1,ExhaustCost),(2,NoCost)]]
    "The Rez" -> [ability s 1 AlterEgoAction OwnsThis ExhaustCost $ runAbility s 1]
    _ -> []
   where
    matcher = W.WindowOneOf
      [W.MakesBasicAttack W.When (IdentityWithTrait T.MartialArtist),W.MakesBasicThwart W.When (IdentityWithTrait T.MartialArtist)
      ,W.MakesBasicDefense W.When (IdentityWithTrait T.MartialArtist) AnyEnemy,W.UsesRecoveryAt W.When (IdentityWithTrait T.MartialArtist)
      ,W.AllyAttacked W.When (AllyWithTrait T.MartialArtist) AnyEnemy,W.AllyThwarted W.When (AllyWithTrait T.MartialArtist) AnyScheme]

instance RunMessage CampaignSupport where
  runMessage msg s@(CampaignSupport attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) n windows _ -> do
      case Name.title (cdName (supportCardDef attrs)) of
        "Precinct HQ" -> do
          minions <- selectListCount $ MinionEngagedWith $ IdentityWithId you
          removeThreat you attrs (1 + minions) ThwartableScheme
        "Air Cover" -> for_ [mid | W.MinionEnteredPlay mid <- windows] $ \mid -> do
          deck <- projectMap PlayerIdentityDeck unDeck you
          discard <- projectMap PlayerIdentityDiscard unDiscard you
          let candidates = filter (cardMatch $ CardWithType UpgradeType <> CardWithTrait T.Tactic) (deck <> discard)
          playable <- filterM (fmap (maybe False (elem $ MinionRef mid)) . getPlayTargets you) candidates
          chooseOne you $ labeled "$label.done" (shuffleDeck you) : [targeting card (addToHand you card >> shuffleDeck you) | card <- playable]
        "Altman Residence" -> do
          heal you 2
          shuffleDiscardMatching you attrs hulklingCard False
        "Hidden Base" -> giveStatus you ToughStatus
        "Foggy Nelson" -> removeThreat you attrs 2 ThwartableScheme
        "Karen Page" -> do
          alter <- identityMatches AlterEgoIdentity you
          shuffleDiscardMatching you attrs daredevilCard False
          when alter $ drawCards you 1
        "Nelson and Murdock" -> for_ [source | W.DefeatedSideSchemeBy _ _ (Just source) <- windows] $ \source -> do
          def <- getsGame (`abilityCardDef` refocus source)
          when (maybe False (member T.Attorney . cdTraits) def) $ chooseEnemyStatus you ConfusedStatus AnyEnemy
        "Sister Maggie" -> chooseStatusRemoval you
        "Stick" -> do
          let identities = [p | w <- windows, p <- case w of W.MadeBasicAttack p -> [p]; W.IdentityThwart p _ -> [p]; W.MadeBasicThwart p -> [p]; W.MadeBasicDefense p _ -> [p]; W.UsedRecovery p -> [p]; _ -> []]
              allies = [a | w <- windows, a <- case w of W.AllyAttack a _ -> [a]; W.AllyThwart a _ -> [a]; _ -> []]
          if n == 1 then do
            traverse_ (`addToBasicPower` 1) identities
            traverse_ (`addToAllyBasicPower` 1) allies
          else do
            traverse_ (\who -> stickPowerPenalty who attrs windows) identities
            traverse_ (`reduceAllyBasicPower` 1) allies
            ready $ supportId attrs
        "The Rez" -> do
          cards <- tuckedPlayerCards you
          defs <- traverse (resolvePlayerCardDef . getCardDef) cards
          let amount = maximum (0 : [fromIntegral $ max 0 cost | def <- defs, Just cost <- [cdCost def]])
          heal you amount
        _ -> pure ()
      pure s
    _ -> CampaignSupport <$> runMessage msg attrs

instance HasModifiersFor CampaignSupport where
  getModifiersFor _ (IdentityRef you) (CampaignSupport attrs)
    | Name.title (cdName (supportCardDef attrs)) == "Sister Maggie", you == supportController attrs = do
        alter <- identityMatches AlterEgoIdentity you
        pure [RecoveryModifier 3 | alter]
  getModifiersFor _ _ _ = pure []

hulklingCard :: CardMatcher
hulklingCard = CardMatchesAny $ map CardIs [Cards.cw56030,Cards.cw56031,Cards.cw56032,Cards.cw56033,Cards.cw56034,Cards.cw56035,Cards.cw56036,Cards.cw56037,Cards.cw56038,Cards.cw56039,Cards.cw56040]

daredevilCard :: CardMatcher
daredevilCard = CardMatchesAny $ map CardIs [Cards.fne60002,Cards.fne60003,Cards.fne60004,Cards.fne60005,Cards.fne60006,Cards.fne60007,Cards.fne60008,Cards.fne60009,Cards.fne60010,Cards.fne60011,Cards.fne60012,Cards.fne60013,Cards.fne60014,Cards.fne60015,Cards.fne60016,Cards.fne60017,Cards.fne60018]
