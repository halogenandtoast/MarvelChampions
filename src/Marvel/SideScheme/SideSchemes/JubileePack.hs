module Marvel.SideScheme.SideSchemes.JubileePack where
import Marvel.Prelude
import Data.List (nub)
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
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T

newtype JubileeSideScheme = JubileeSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
jubilee47003, jubilee47016, jubilee47025, jubilee47031, jubilee47032, jubilee47033 :: SideSchemeCard JubileeSideScheme
jubilee47003 = sideScheme JubileeSideScheme Cards.jubilee47003 (Static 2)
jubilee47016 = sideScheme JubileeSideScheme Cards.jubilee47016 (PerPlayer 3)
jubilee47025 = sideScheme JubileeSideScheme Cards.jubilee47025 (PerPlayer 2)
jubilee47031 = sideScheme JubileeSideScheme Cards.jubilee47031 (PerPlayer 2)
jubilee47032 = sideScheme JubileeSideScheme Cards.jubilee47032 (PerPlayer 2)
jubilee47033 = sideScheme JubileeSideScheme Cards.jubilee47033 (PerPlayer 2)

instance IsSideScheme JubileeSideScheme where
  sideSchemeAbilities = getAbilities

instance HasModifiersFor JubileeSideScheme where
  getModifiersFor _ (SideSchemeRef sid) (JubileeSideScheme attrs) | sid == sideSchemeId attrs = pure $ case toCardCode attrs of
    "47003" -> [ThreatCannotBeRemovedByHeroOrAlly]
    "47016" -> [BasicThwartModifierForTrait T.XMen 1]
    _ -> []
  getModifiersFor _ _ _ = pure []
instance HasAbilities JubileeSideScheme where
  getAbilities a@(JubileeSideScheme attrs)
    | toCardCode attrs == "47003" = [label "$label.jubileeShoppingSpree" $ ability a 1 AlterEgoAction NoCriteria (ExhaustCharacterCost $ IdentityCharacters You) (runAbility a 1)]
    | otherwise = []
instance RunMessage JubileeSideScheme where
  runMessage msg a@(JubileeSideScheme attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      thwartSchemeAs you (sideSchemeId attrs) attrs 1
      pure a
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      when (toCardCode attrs == "47025") do
        you <- getYou
        hand <- projectMap PlayerIdentityHand unHand you
        let n = fromIntegral . length . nub $ concatMap (printedResources . getCardDef) hand
        placeThreatOn attrs (sideSchemeId attrs) n
      pure a
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      cardSpecific attrs "whenDefeated" you
      pure a
    -- Elaborate Trap resolves this ability without defeating or removing the scheme.
    CardSpecific (isTarget attrs -> True) "whenDefeated" (fromJSON -> Success you) -> do
      case toCardCode attrs of
        "47003" -> cardSpecific attrs "searchItem" you
        "47016" -> getPlayers >>= traverse_ (cardSpecific attrs "searchEvent")
        "47031" -> damageIdentity you attrs (toDamage 2 FromAbility)
        "47032" -> do
          stunned <- identityMatches StunnedIdentity you
          if not stunned then stunIdentity you else do
            allies <- selectList (AllyControlledBy (IdentityWithId you) <> AllyThatCanLeavePlay)
            upgrades <- selectList (UpgradeControlledBy (IdentityWithId you) <> UpgradeThatCanBeDiscarded)
            chooseOne you ([targeting (AllyRef aid) $ removeFromPlay (AllyRef aid) | aid <- allies]
              <> [targeting (UpgradeRef uid) $ discardUpgrade uid | uid <- upgrades])
        "47033" -> do
          confused <- identityMatches ConfusedIdentity you
          if confused then placeThreatOnEach attrs 2 EncounterMainScheme else confuseIdentity you
        _ -> pure ()
      pure a
    CardSpecific (isTarget attrs -> True) "searchItem" (fromJSON -> Success you) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      allowed <- selectList (BasicCardMatches (CardWithTrait T.Item) <> CanEnterPlay)
      cards <- filterM (fmap (maybe True notNull) . getPlayTargets you) $ filter (`elem` allowed) (deck <> discard)
      focusing (PlayerCard <$> cards) $ chooseOne you [targeting card $ cardSpecific attrs "putItem" (you,card) | card <- cards]
      shuffleDeck you
      pure a
    CardSpecific (isTarget attrs -> True) "putItem" (fromJSON -> Success (you,card)) -> do
      targets <- getPlayTargets you card
      case targets of
        Nothing -> putIntoPlay you card
        Just ts -> chooseOrRunOne you [targeting target $ putIntoPlayOn you card target | target <- ts]
      pure a
    CardSpecific (isTarget attrs -> True) "searchEvent" (fromJSON -> Success you) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      let cards = filter (cardMatch (HeroSpecificCard <> CardWithType EventType)) (deck <> discard)
      focusing (PlayerCard <$> cards) $ chooseOne you (labeled "$label.noResponse" (pure ()) : [targeting card $ addToHand you card | card <- cards])
      shuffleDeck you
      pure a
    _ -> JubileeSideScheme <$> runMessage msg attrs
