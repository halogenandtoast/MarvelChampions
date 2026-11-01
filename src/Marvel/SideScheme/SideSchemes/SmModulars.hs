module Marvel.SideScheme.SideSchemes.SmModulars where

import Marvel.Prelude
import Marvel.Attachment.Types qualified as Attachment ()
import Marvel.Keyword qualified as Keyword
import Marvel.Ability
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.Effect.Types
import Marvel.Damage ()
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait (Trait (Civilian, Location, Persona, Tech))
import Marvel.Card
import Marvel.Count
import Marvel.EncounterSet (getNemesisSet)
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (PlayerIdentitySide (..), Field (PlayerIdentityHand, PlayerIdentitySide, PlayerIdentitySides))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp ()
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types hiding (SideSchemeId)

newtype SmModularsSideScheme = SmModularsSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smPanicInTheStreets :: SideSchemeCard SmModularsSideScheme
smPanicInTheStreets = sideSchemeWith SmModularsSideScheme Cards.smPanicInTheStreets (Static 5) (crisisL .~ False)

smVolunteerWork :: SideSchemeCard SmModularsSideScheme
smVolunteerWork = sideSchemeWith SmModularsSideScheme Cards.smVolunteerWork (PerPlayer 3) (crisisL .~ False)

smLimitlessSupply :: SideSchemeCard SmModularsSideScheme
smLimitlessSupply = sideSchemeWith SmModularsSideScheme Cards.smLimitlessSupply (Static 5) (crisisL .~ False)

smCoordinatedEffort :: SideSchemeCard SmModularsSideScheme
smCoordinatedEffort = sideSchemeWith SmModularsSideScheme Cards.smCoordinatedEffort (Static 6) (crisisL .~ False)

smHiddenInShadow :: SideSchemeCard SmModularsSideScheme
smHiddenInShadow = sideSchemeWith SmModularsSideScheme Cards.smHiddenInShadow (Static 4) (crisisL .~ False)

smTeamworkMakesTheDreamWork :: SideSchemeCard SmModularsSideScheme
smTeamworkMakesTheDreamWork = sideSchemeWith SmModularsSideScheme Cards.smTeamworkMakesTheDreamWork (Static 5) (crisisL .~ False)

smFoolSParadise :: SideSchemeCard SmModularsSideScheme
smFoolSParadise = sideSchemeWith SmModularsSideScheme Cards.smFoolSParadise (PerPlayer 6) (crisisL .~ True)

smWeaknessFromWithin :: SideSchemeCard SmModularsSideScheme
smWeaknessFromWithin = sideSchemeWith SmModularsSideScheme Cards.smWeaknessFromWithin (Static 1) (crisisL .~ False)

smAnalysisParalysis :: SideSchemeCard SmModularsSideScheme
smAnalysisParalysis = sideSchemeWith SmModularsSideScheme Cards.smAnalysisParalysis (Static 1) (crisisL .~ False)

instance IsSideScheme SmModularsSideScheme where
  toSideSchemeAttrs (SmModularsSideScheme attrs) = attrs
  sideSchemeAbilities a@(SmModularsSideScheme attrs)
    | toCardCode attrs == "27133" = [ability a 1 AlterEgoAction NoCriteria (MultiResourceCost [Nothing, Nothing]) (runAbility a 1)]
    | otherwise = []

instance HasModifiersFor SmModularsSideScheme where
  getBlankingModifiersFor _ (SupportRef sid) (SmModularsSideScheme attrs) | toCardCode attrs == "27127" = do
    location <- member sid <$> select (SupportWithTrait Location)
    persona <- member sid <$> select (SupportWithTrait Persona)
    pure [Blank | location || persona]
  getBlankingModifiersFor _ _ _ = pure []
  getModifiersFor _ (AttachmentRef aid) (SmModularsSideScheme attrs) | toCardCode attrs == "27140" = do
    def <- getsGame (`abilityCardDef` AttachmentRef aid)
    pure [KeywordModifier Keyword.Surge | maybe False (member Tech . cdTraits) def]
  getModifiersFor _ (SideSchemeRef sid) (SmModularsSideScheme attrs)
    | sid == sideSchemeId attrs, toCardCode attrs == "27133" = pure [ThreatCannotBeRemovedByThwarting]
  getModifiersFor _ target (SmModularsSideScheme attrs)
    | isEnemy target, toCardCode attrs == "27143" = pure [AccelerationIconModifier 1]
    | isEnemy target, toCardCode attrs == "27144" = pure [HazardIconModifier 1]
    | isEnemy target, toCardCode attrs == "27145" = pure [AttackModifier 1, SchemeModifier 1]
   where
    isEnemy (MinionRef _) = True
    isEnemy (VillainRef _) = True
    isEnemy _ = False
  getModifiersFor _ (IdentityRef _) (SmModularsSideScheme attrs) | toCardCode attrs == "27155" = pure [HandSizeModifier 2]
  getModifiersFor source target a = getBlankingModifiersFor source target a

instance RunMessage SmModularsSideScheme where
  runMessage msg s@(SmModularsSideScheme attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "27133" -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      case lookup side sides of
        Just (AlterEgoSide alterEgo) -> do
          recovery <- AlterEgo.getModifiedRecovery (toAttrs alterEgo)
          thwartScheme (sideSchemeId attrs) attrs recovery
          civilian <- identityMatches (IdentityWithTrait Civilian) you
          when civilian $ drawCards you 1
        _ -> pure ()
      pure s
    Boost (RevealedAsBoost target _) | isTarget attrs target, toCardCode attrs == "27143" -> do
      difficulty <- getDifficulty
      schemes <- selectList AnyScheme
      mains <- selectList EncounterMainScheme
      for_ schemes \sid -> placeThreat sid (if difficulty == Expert && sid `elem` mains then 2 else 1)
      pure s
    Boost (RevealedAsBoost target _) | isTarget attrs target, toCardCode attrs == "27144" -> do
      difficulty <- getDifficulty
      players <- getPlayers
      first <- selectOne FirstPlayer
      for_ players \you -> dealIndirectDamage you attrs (if difficulty == Expert && Just you == first then 2 else 1)
      pure s
    Boost (RevealedAsBoost target _) | isTarget attrs target, toCardCode attrs == "27145" -> do
      difficulty <- getDifficulty
      when (difficulty == Expert) $ createEffect Cards.smTeamworkMakesTheDreamWork attrs (IdentityEntity AnyIdentity)
      pure s
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "27156" -> do
      you <- getYou
      n <- projectMap PlayerIdentityHand (fromIntegral . length . unHand) you
      when (n > 0) (placeThreat sid n)
      pure s
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "27173" -> do
      you <- getYou
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      let identityCode = case lookup side sides of
            Just (HeroSide hero) -> toCardCode hero
            Just (AlterEgoSide alterEgo) -> toCardCode alterEgo
            Nothing -> error "identity has no current side"
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      aside <- getSetAsideEncounterCards
      let cards = deck <> discard <> aside
      let found = filter (cardMatch $ CardInEncounterSet (getNemesisSet identityCode) <> CardWithType SideSchemeType) cards
      unless (null found) $ chooseOrRunOne you
        [targeting card do
            revealEncounterCardFrom (if card `elem` discard then W.RevealedFromDiscardPile else if card `elem` aside then W.RevealedFromSetAside else W.RevealedFromEncounterDeck) you card
            cardSpecific attrs "nemesisSchemeThreat" (SideSchemeId $ unCardId $ ecCardId card)
        | card <- found]
      shuffleEncounterDeck
      pure s
    CardSpecific (isTarget attrs -> True) "nemesisSchemeThreat" (fromJSON -> Success sid) -> do
      present <- selectAny (SideSchemeWithId sid)
      when present do
        n <- selectCount SchemeThreat (SchemeWithId $ SchemeSideSchemeId sid)
        when (n > 0) $ placeThreat (sideSchemeId attrs) n
      pure s
    _ -> SmModularsSideScheme <$> runMessage msg attrs

-- The bonus belongs to this physical card's count. An effect disables itself after
-- that count, so responding and reopening its window cannot add it twice.
newtype TeamworkMakesTheDreamWorkBoostEffect = TeamworkMakesTheDreamWorkBoostEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

teamworkMakesTheDreamWorkBoostEffect :: CardEffect TeamworkMakesTheDreamWorkBoostEffect
teamworkMakesTheDreamWorkBoostEffect = effect TeamworkMakesTheDreamWorkBoostEffect Cards.smTeamworkMakesTheDreamWork

instance RunMessage TeamworkMakesTheDreamWorkBoostEffect where
  runMessage msg e@(TeamworkMakesTheDreamWorkBoostEffect attrs) = runQueueT $ case msg of
    CheckWindows windows | SideSchemeRef sid <- effectSource attrs
      , cards@(_ : _) <- [card | W.Window W.When (W.CountingBoostIcons card) <- windows, ecCardId card == CardId (unSideSchemeId sid)] -> do
          for_ cards \card -> changeBoostIconCount card (+ 2)
          disableEffect (effectId attrs)
          pure e
    ClearBoosts -> e <$ disableEffect (effectId attrs)
    _ -> TeamworkMakesTheDreamWorkBoostEffect <$> runMessage msg attrs
