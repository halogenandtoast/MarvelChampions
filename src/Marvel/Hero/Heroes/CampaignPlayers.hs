{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Hero.Heroes.CampaignPlayers where
import Marvel.Prelude
import Data.Maybe (maybeToList)
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
import Marvel.CampaignPlayers.Helpers
import Marvel.FalconWinter.Helpers (nextCardReduction)
import Marvel.Hero.Runner

cw56001a :: HeroCard CampaignHero
cw56001a = hero (\attrs -> CampaignHero attrs [] Nothing Nothing) Cards.cw56001a (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 2) (Def 3)

cw56029a :: HeroCard CampaignHero
cw56029a = hero (\attrs -> CampaignHero attrs [] Nothing Nothing) Cards.cw56029a (HP $ Static 13) (HandSize 5) (Thw 1) (Atk 1) (Def 1)

fne60001a :: HeroCard CampaignHero
fne60001a = hero (\attrs -> CampaignHero attrs [] Nothing Nothing) Cards.fne60001a (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 2) (Def 2)

fne60037a :: HeroCard CampaignHero
fne60037a = hero (\attrs -> CampaignHero attrs [] Nothing Nothing) Cards.fne60037a (HP $ Static 9) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

data CampaignHero = CampaignHero
  { campaignHeroAttrs :: Attrs Hero
  , campaignEchoCards :: [PlayerCard]
  , campaignEchoSuppressed :: Maybe CardId
  , campaignEchoReserved :: Maybe (PlayerCard, PlayerCard)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
instance IsHero CampaignHero where toHeroAttrs = campaignHeroAttrs
instance IsRef CampaignHero where toRef = toRef . campaignHeroAttrs
instance HasCardCode CampaignHero where toCardCode = toCardCode . campaignHeroAttrs

instance HasAbilities CampaignHero where
  getAbilities h@CampaignHero {campaignHeroAttrs=attrs,campaignEchoSuppressed=suppressed} = case toCardCode attrs of
    "56001a" -> [label "$label.onTheHunt" $ limitedWindowAbility h 1 (W.PhaseBegins W.After PlayerPhase) Response (IsSelf <> MinionExists (MinionEngagedWith $ IdentityWithId $ heroIdentityId attrs)) NoCost $ runAbility h 1]
    "56029a" -> [label "$label.chosenShape" $ limitedWindowAbility h 1 (W.PlayerCardEntersPlay W.After you $ CardWithTrait T.Shapeshift) ForcedResponse IsSelf NoCost $ runAbility h 1]
    "60001a" -> [label "$label.superhumanSenses" $ ability h 1 Action (IsSelf <> CanPlayTopSense) NoCost $ runAbility h 1]
    "60037a" -> [label "$label.watchAndLearn" $ limitedWindowAbility h 1 (W.PlaysEvent W.After AnyIdentity $ CardMatchesAny [CardWithAspect a | a <- [Aspect.Leadership,Aspect.Aggression,Aspect.Protection,Aspect.Justice,Aspect.Pool,Aspect.Neutral]]) Response (IsSelf <> PlayedEventInDiscard <> maybe NoCriteria PlayedEventIsNot suppressed) NoCost $ runAbility h 1]
    _ -> []
   where you = IdentityWithId $ heroIdentityId attrs

instance HasModifiersFor CampaignHero where
  getModifiersFor _ target CampaignHero {campaignHeroAttrs=attrs}
    | toCardCode attrs == "56001a" = case target of
        MinionRef mid -> do
          hunted <- enemyMatches (EnemyWithUpgrade $ UpgradeIs Cards.cw56007) $ EnemyMinionId mid
          pure [LoseKeyword K.Guard | hunted]
        EnemyRef enemy -> do
          hunted <- enemyMatches (EnemyWithUpgrade $ UpgradeIs Cards.cw56007) enemy
          pure [LoseKeyword K.Guard | hunted]
        _ -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage CampaignHero where
  runMessage msg h@CampaignHero {campaignHeroAttrs=attrs,campaignEchoCards=cards,campaignEchoSuppressed=suppressed} = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case toCardCode attrs of
        "56001a" -> selectListCount (MinionEngagedWith $ IdentityWithId you) >>= drawCards you . min 3
        "56029a" -> do
          let entered = [UpgradeId $ unCardId $ pcCardId card | W.PlayerCardEnteredPlay _ card <- windows]
          shapes <- selectList $ UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait T.Shapeshift
          traverse_ discardUpgrade $ filter (`notElem` entered) shapes
          ready you
        "60001a" -> do
          deck <- identitySenseDeck you
          for_ (headMay deck) $ \card -> playFromSenseDeck you card False
        "60037a" -> for_ [eid | W.EventPlayed _ eid _ <- windows] $ \eid -> do
          card <- getEventPlayerCard eid
          for_ card $ \played -> when (Just (pcCardId played) /= suppressed) $ do
            discard <- projectMap PlayerIdentityDiscard unDiscard $ fromMaybe you $ pcOwner played
            when (any ((== pcCardId played) . pcCardId) discard) $ tuckPlayerCard you played
        _ -> pure ()
      pure h
    CardSpecific (IdentityRef you) "fneTuckPlayerCard" (fromJSON -> Success card)
      | you == heroIdentityId attrs, toCardCode attrs == "60037a" -> do
          cardSpecific you "fneTrimTucked" ()
          pure h {campaignEchoCards = filter ((/= pcCardId card) . pcCardId) cards <> [card]}
    CardSpecific (IdentityRef you) "fneTrimTucked" _
      | you == heroIdentityId attrs, toCardCode attrs == "60037a" -> do
          when (length cards > 3) $ chooseOne you [targeting card (discardTuckedPlayerCard you card >> cardSpecific you "fneTrimTucked" ()) | card <- cards]
          pure h
    CardSpecific (IdentityRef you) "fneReleaseTuckedCard" (fromJSON -> Success card)
      | you == heroIdentityId attrs -> pure h {campaignEchoCards = filter ((/= pcCardId card) . pcCardId) cards}
    CardSpecific (IdentityRef you) "fneReserveTuckedEvent" (fromJSON -> Success (card, isSuppressed))
      | you == heroIdentityId attrs -> pure h {campaignEchoCards = filter ((/= pcCardId card) . pcCardId) cards, campaignEchoSuppressed = if isSuppressed then Just $ pcCardId card else Nothing}
    CardSpecific (IdentityRef you) "fneReserveReplay" (fromJSON -> Success (card,reflexes))
      | you == heroIdentityId attrs -> pure h {campaignEchoCards = filter ((/= pcCardId card) . pcCardId) cards, campaignEchoSuppressed = Just $ pcCardId card, campaignEchoReserved = Just (card,reflexes)}
    CardSpecific (IdentityRef you) "fneRestoreReplay" (fromJSON -> Success (card,_reflexes :: PlayerCard))
      | you == heroIdentityId attrs -> pure h {campaignEchoCards = filter ((/= pcCardId card) . pcCardId) cards <> [card], campaignEchoSuppressed = Nothing, campaignEchoReserved = Nothing}
    CardSpecific (IdentityRef you) "fneReplayFinished" _
      | you == heroIdentityId attrs -> pure h {campaignEchoSuppressed = Nothing, campaignEchoReserved = Nothing}
    _ -> do
      attrs' <- runMessage msg attrs
      pure h {campaignHeroAttrs = attrs'}
