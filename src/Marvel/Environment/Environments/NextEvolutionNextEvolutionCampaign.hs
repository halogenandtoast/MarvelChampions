module Marvel.Environment.Environments.NextEvolutionNextEvolutionCampaign where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Discard
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.PlayerCard (lookupPlayerCard)
import Marvel.Projection
import Marvel.Query
import Marvel.Support.Cards qualified as Supports
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NextEvolutionCampaignEnvironment = NextEvolutionCampaignEnvironment (Attrs Environment)
  deriving anyclass IsEnvironment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40190b :: EnvironmentCard NextEvolutionCampaignEnvironment
ne40190b = environment NextEvolutionCampaignEnvironment Cards.ne40190b

ne40191b :: EnvironmentCard NextEvolutionCampaignEnvironment
ne40191b = environment NextEvolutionCampaignEnvironment Cards.ne40191b

ne40192b :: EnvironmentCard NextEvolutionCampaignEnvironment
ne40192b = environment NextEvolutionCampaignEnvironment Cards.ne40192b

ne40193b :: EnvironmentCard NextEvolutionCampaignEnvironment
ne40193b = environment NextEvolutionCampaignEnvironment Cards.ne40193b

ne40194b :: EnvironmentCard NextEvolutionCampaignEnvironment
ne40194b = environment NextEvolutionCampaignEnvironment Cards.ne40194b

ne40195b :: EnvironmentCard NextEvolutionCampaignEnvironment
ne40195b = environment NextEvolutionCampaignEnvironment Cards.ne40195b

instance HasModifiersFor NextEvolutionCampaignEnvironment where
  getModifiersFor _ target (NextEvolutionCampaignEnvironment a) = case target of
    IdentityRef you -> do
      hero <- selectAny (IdentityWithId you <> HeroIdentity)
      pure $ case toCardCode a of
        "40194b" -> [ResourceCostReductionFor (CardWithType EventType <> CardWithPrintedCostAtLeast 3) 1]
        "40195b" | hero -> [DefenseModifier 1, KeywordModifier $ Retaliate 1]
        _ -> []
    _ -> pure []

instance HasAbilities NextEvolutionCampaignEnvironment where
  getAbilities (NextEvolutionCampaignEnvironment a) =
    [ability a 1 Action ForAnyPlayer (ExactCountersCost 1) (runAbility a 1)
    | toCardCode a `elem` ["40190b", "40191b", "40192b", "40193b"]]

instance RunMessage NextEvolutionCampaignEnvironment where
  runMessage msg e@(NextEvolutionCampaignEnvironment a) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId a ->
      pure $ NextEvolutionCampaignEnvironment a
        {environmentCounters = if toCardCode a `elem` ["40190b", "40191b", "40192b", "40193b"] then 1 else 0}
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      players <- getPlayers
      case toCardCode a of
        "40190b" -> for_ players \you -> cardSpecific a "neRewardSearch" (you, AllyType, 3 :: Natural, True)
        "40191b" -> do
          first <- selectJust FirstPlayer
          putIntoPlay first =<< newRewardCard first Supports.ne40197
        "40192b" -> for_ players \you -> shuffleIntoDeck you . pure =<< newRewardCard you (lookupPlayerCard "40196")
        "40193b" -> for_ players \you -> cardSpecific a "neRewardSearch" (you, UpgradeType, 2 :: Natural, False)
        _ -> pure ()
      pure e
    CardSpecific target "neRewardSearch" (fromJSON -> Success (you, kind, maximumCost, optional)) | target == toRef a -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      let matcher = CardWithType kind <> CardWithPrintedCostAtMost maximumCost
      legal <- selectList (CanEnterPlay <> BasicCardMatches matcher)
      cards <- filterM (fmap (maybe True notNull) . getPlayTargets you) $ filter (`elem` legal) (deck <> discard)
      unless (null cards) $ chooseOneSearchedIn you
        (if kind == AllyType then "$label.neCampaignSearchAlly" else "$label.neCampaignSearchUpgrade")
        [("$label.deck", map PlayerCard deck), ("$label.discardPile", map PlayerCard discard)]
        ([targeting card (cardSpecific a "neRewardPutCard" (you,card)) | card <- cards]
          <> [labeled "$label.pass" (pure ()) | optional])
      shuffleDeck you
      pure e
    CardSpecific target "neRewardPutCard" (fromJSON -> Success (you,card)) | target == toRef a -> do
      targets <- getPlayTargets you card
      case targets of
        Nothing -> putIntoPlay you card
        Just ts -> chooseOrRunOne you [targeting destination (putIntoPlayOn you card destination) | destination <- ts]
      pure e
    _ -> NextEvolutionCampaignEnvironment <$> runMessage msg a

newRewardCard :: MonadRandom m => IdentityId -> CardDef -> m PlayerCard
newRewardCard you def = do
  cid <- getRandom
  pure $ MkPlayerCard cid def (Just you) (Just you)
