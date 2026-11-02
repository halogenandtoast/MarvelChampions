{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.AlterEgo.AlterEgos.CampaignPlayers where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Data.List (findIndex)
import Marvel.Ability
import Marvel.Aspect qualified as Aspect
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
import Marvel.CampaignPlayers.EncounterCards qualified as Encounter
import Marvel.CampaignPlayers.Helpers
import Marvel.FalconWinter.Helpers (nextCardReduction)
import Marvel.AlterEgo.Runner

cw56001b :: AlterEgoCard CampaignAlterEgo
cw56001b = alterEgo (\attrs -> CampaignAlterEgo attrs [] Nothing) Cards.cw56001b (HP $ Static 10) (HandSize 6) (Rec 3) [Encounter.enc56023]

cw56029b :: AlterEgoCard CampaignAlterEgo
cw56029b = alterEgo (\attrs -> CampaignAlterEgo attrs [] Nothing) Cards.cw56029b (HP $ Static 13) (HandSize 5) (Rec 4) [Encounter.enc56054]

fne60001b :: AlterEgoCard CampaignAlterEgo
fne60001b = alterEgo (\attrs -> CampaignAlterEgo attrs [] Nothing) Cards.fne60001b (HP $ Static 10) (HandSize 6) (Rec 3) [Encounter.enc60032]

fne60037b :: AlterEgoCard CampaignAlterEgo
fne60037b = alterEgo (\attrs -> CampaignAlterEgo attrs [] Nothing) Cards.fne60037b (HP $ Static 9) (HandSize 6) (Rec 3) [Encounter.enc60060]

data CampaignAlterEgo = CampaignAlterEgo
  { campaignAlterEgoAttrs :: Attrs AlterEgo
  , campaignSenseCards :: [PlayerCard]
  , campaignSensePending :: Maybe (PlayerCard, Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasModifiersFor)
instance IsAlterEgo CampaignAlterEgo where toAlterEgoAttrs = campaignAlterEgoAttrs
instance IsRef CampaignAlterEgo where toRef = toRef . campaignAlterEgoAttrs
instance HasCardCode CampaignAlterEgo where toCardCode = toCardCode . campaignAlterEgoAttrs

instance HasAbilities CampaignAlterEgo where
  getAbilities h@CampaignAlterEgo {campaignAlterEgoAttrs=attrs} = case toCardCode attrs of
    "56001b" -> [label "$label.undercoverWork" $ limitedAbility h 1 (PerRound 1) Action IsSelf NoCost $ runAbility h 1]
    "56029b" -> [label "$label.shapeChanger" $ limitedAbility h 1 (PerRound 1) Action IsSelf NoCost $ runAbility h 1]
    "60001b" -> [ability h 1 Setup NoCriteria NoCost $ runAbility h 1]
    "60037b" -> [label "$label.practiceMakesPerfect" $ limitedWindowAbility h 1 (W.IdentityChangedToForm W.When $ IdentityWithId $ alterEgoIdentityId attrs) Interrupt IsSelf NoCost $ runAbility h 1]
    _ -> []

instance RunMessage CampaignAlterEgo where
  runMessage msg h@CampaignAlterEgo {campaignAlterEgoAttrs=attrs,campaignSenseCards=cards} = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      case toCardCode attrs of
        "56001b" -> searchOwned you attrs (CardIs Cards.cw56007) True
        "56029b" -> searchOwned you attrs (CardWithTrait T.Shapeshift) True
        "60001b" -> setupSenseDeck you [Cards.fne60002, Cards.fne60003, Cards.fne60004, Cards.fne60005, Cards.fne60006]
        "60037b" -> searchOwned you attrs (CardWithType EventType <> CardMatchesAny [CardWithAspect a | a <- [Aspect.Leadership,Aspect.Aggression,Aspect.Protection,Aspect.Justice,Aspect.Pool,Aspect.Neutral]]) False
        _ -> pure ()
      pure h
    CardSpecific (IdentityRef you) "fneSetupSenseDeck" (fromJSON -> Success newCards)
      | you == alterEgoIdentityId attrs, toCardCode attrs == "60001b" -> pure h {campaignSenseCards = newCards}
    CardSpecific (IdentityRef you) "fneReserveSenseCard" (fromJSON -> Success card)
      | you == alterEgoIdentityId attrs -> pure h {campaignSenseCards = filter (/= card) cards}
    CardSpecific (IdentityRef you) "fneSenseReturned" (fromJSON -> Success card)
      | you == alterEgoIdentityId attrs, toCardCode attrs == "60001b" -> pure h {campaignSenseCards = filter (/= card) cards <> [card]}
    CardSpecific (IdentityRef you) "fneReserveSensePlay" (fromJSON -> Success card)
      | you == alterEgoIdentityId attrs -> do
          let index = fromMaybe 0 $ findIndex ((== pcCardId card) . pcCardId) cards
          pure h {campaignSenseCards = filter ((/= pcCardId card) . pcCardId) cards, campaignSensePending = Just (card,index)}
    CardSpecific (IdentityRef you) "fneRestoreSensePlay" (fromJSON -> Success (card,index :: Int))
      | you == alterEgoIdentityId attrs -> do
          let remaining = filter ((/= pcCardId card) . pcCardId) cards
          pure h {campaignSenseCards = take index remaining <> [card] <> drop index remaining, campaignSensePending = Nothing}
    CardSpecific (IdentityRef you) "fneSensePlayFinished" _
      | you == alterEgoIdentityId attrs -> pure h {campaignSensePending = Nothing}
    _ -> do
      attrs' <- runMessage msg attrs
      pure h {campaignAlterEgoAttrs = attrs'}
