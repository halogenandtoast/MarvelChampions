module Marvel.MainScheme.MainSchemes.MutantGenesis where

import Marvel.Id ()
import Marvel.Prelude
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Deck (unDeck)
import Marvel.MainScheme.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Projection
import Marvel.Modifier
import Marvel.Keyword
import Marvel.Trait qualified as Trait
import Marvel.Damage
import Marvel.Ally.Cards qualified as Allies
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.Environment.Cards qualified as Environments
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Minion.Cards qualified as Minions
import Marvel.Window qualified as W ()
import Marvel.Phase
import Marvel.Ability ()
import Marvel.Cost ()
import Marvel.Question ()
import Marvel.MainScheme.Cards qualified as Cards

newtype MutantGenesisMainScheme = MutantGenesisMainScheme (Attrs MainScheme)
    deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mgStalkedBySabretooth :: MainSchemeCard MutantGenesisMainScheme
mgStalkedBySabretooth = mainScheme MutantGenesisMainScheme Cards.mgStalkedBySabretooth (PerPlayer 0) (PerPlayer 0) (PerPlayer 1)

mgTheInjuredSenator :: MainSchemeCard MutantGenesisMainScheme
mgTheInjuredSenator = mainScheme MutantGenesisMainScheme Cards.mgTheInjuredSenator (PerPlayer 9) (PerPlayer 0) (PerPlayer 1)

mgNightOfTheSentinels :: MainSchemeCard MutantGenesisMainScheme
mgNightOfTheSentinels = mainScheme MutantGenesisMainScheme Cards.mgNightOfTheSentinels (PerPlayer 0) (PerPlayer 1) (PerPlayer 1)

mgTheSentinelFactory :: MainSchemeCard MutantGenesisMainScheme
mgTheSentinelFactory = mainScheme MutantGenesisMainScheme Cards.mgTheSentinelFactory (PerPlayer 6) (PerPlayer 1) (PerPlayer 1)

mgMasterMoldsAgenda :: MainSchemeCard MutantGenesisMainScheme
mgMasterMoldsAgenda = mainScheme MutantGenesisMainScheme Cards.mgMasterMoldsAgenda (PerPlayer 8) (PerPlayer 1) (PerPlayer 1)

mgTheBrotherhoodStrikes :: MainSchemeCard MutantGenesisMainScheme
mgTheBrotherhoodStrikes = mainScheme MutantGenesisMainScheme Cards.mgTheBrotherhoodStrikes (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

mgTheAtrium :: MainSchemeCard MutantGenesisMainScheme
mgTheAtrium = mainScheme MutantGenesisMainScheme Cards.mgTheAtrium (PerPlayer 7) (PerPlayer 1) (PerPlayer 1)

mgTheCafeteria :: MainSchemeCard MutantGenesisMainScheme
mgTheCafeteria = mainScheme MutantGenesisMainScheme Cards.mgTheCafeteria (PerPlayer 7) (PerPlayer 1) (PerPlayer 1)

mgTheBasketballCourt :: MainSchemeCard MutantGenesisMainScheme
mgTheBasketballCourt = mainScheme MutantGenesisMainScheme Cards.mgTheBasketballCourt (PerPlayer 7) (PerPlayer 1) (PerPlayer 1)

mgTheCourtyard :: MainSchemeCard MutantGenesisMainScheme
mgTheCourtyard = mainScheme MutantGenesisMainScheme Cards.mgTheCourtyard (PerPlayer 7) (PerPlayer 1) (PerPlayer 1)

mgAsteroidM :: MainSchemeCard MutantGenesisMainScheme
mgAsteroidM = mainScheme MutantGenesisMainScheme Cards.mgAsteroidM (PerPlayer 5) (PerPlayer 1) (PerPlayer 1)

mgFactoryOnline :: MainSchemeCard MutantGenesisMainScheme
mgFactoryOnline = mainScheme MutantGenesisMainScheme Cards.mgFactoryOnline (PerPlayer 6) (PerPlayer 1) (PerPlayer 1)

mgTheRuleOfMagnus :: MainSchemeCard MutantGenesisMainScheme
mgTheRuleOfMagnus = mainScheme MutantGenesisMainScheme Cards.mgTheRuleOfMagnus (PerPlayer 7) (PerPlayer 1) (PerPlayer 1)

instance IsMainScheme MutantGenesisMainScheme where
  mainSchemeModifiersFor _ target (MutantGenesisMainScheme a) = case toCardCode a of
    "32087" | SideSchemeRef sid <- target -> do
      zero <- elem sid <$> selectList (SideSchemeIs Sides.mgOperationZeroTolerance)
      pure [CannotLeavePlay | zero]
    code | code `elem` ["32112", "32113"], MinionRef mid <- target -> do
      sentinel <- selectAny (MinionWithId mid <> MinionWithTrait Trait.Sentinel)
      pure [KeywordModifier Guard | sentinel]
    "32126" | isCharacter target -> pure [KeywordModifier Steady]
    "32127" | isCharacter target -> pure [KeywordModifier $ Retaliate 1]
    "32128" | AllyRef _ <- target -> pure [KeywordModifier Toughness]
    "32128" | MinionRef _ <- target -> pure [KeywordModifier Toughness]
    "32129" | isCharacter target -> pure [AttackModifier 1]
    _ -> pure []
   where
    isCharacter = \case IdentityRef _ -> True; AllyRef _ -> True; VillainRef _ -> True; MinionRef _ -> True; _ -> False

instance RunMessage MutantGenesisMainScheme where
  runMessage msg e@(MutantGenesisMainScheme attrs) = runQueueT $ case msg of
    VillainPhaseStep StepEnemiesActivate | toCardCode attrs == "32063" -> do
      players <- getPlayerCount
      let amount = if mainSchemeThreat attrs >= 6 * fromIntegral players then 3 else 2
      senators <- selectList (AllyIs Allies.mgRobertKelly)
      forcedResponse attrs $ for_ senators (\aid -> damageCharacter aid attrs $ toDamage amount FromAbility)
      pure e
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      case toCardCode attrs of
        "32063" -> do
          putEncounterCardIntoPlay first =<< genEncounterCard Sides.mgFindTheSenator
          cardSpecific GameRef "mgSetupSenator" ()
          cardSpecific attrs "mgChooseSenatorDamage" ()
        "32087" -> do
          searchAndRevealEncounterCard first (CardIs Sides.mgOperationZeroTolerance)
          revealEncounterCard first =<< genEncounterCard Sides.mgMutantsAtTheMall
          cardSpecific GameRef "mgSetAsideCaptives" ()
          cardSpecific attrs "mgChooseCapturedCards" ()
        "32112" -> do
          cardSpecific GameRef "mgPutMagnetoAlly" ()
        "32125" -> putEncounterCardIntoPlay first =<< genEncounterCard Environments.mgSaveTheSchool
        "32141" -> do
          setAside . map EncounterCard =<< traverse genEncounterCard [Sides.mgOrbitalDecay]
          revealEncounterCard first =<< genEncounterCard Sides.mgBoardingParty
        _ -> pure ()
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId attrs -> do
      first <- selectJust FirstPlayer
      case toCardCode attrs of
        "32064" -> getPlayers >>= traverse_ dealEncounterCard
        "32112" -> revealSentinels
        "32113" -> do
          shuffleIntoEncounterDeck =<< getEncounterDiscard
          cardSpecific attrs "revealSentinels" ()
        "32125" -> do
          getPlayers >>= traverse_ dealEncounterCard
          addToVictoryDisplay attrs
          nextMainScheme mid
        "32142" -> do
          placeCounters attrs 1
          victory <- getVictoryDisplay
          unless (any (cardMatch $ CardIs Sides.mgSabotageMasterMold) victory) $ searchAndRevealEncounterCard first (CardIs Minions.mgMTypeSentinel)
        "32143" -> do
          placeCounters attrs 2
          physical <- selectAny (AttachmentIs Attachments.mgPhysicalStrain)
          unless physical $ searchAndRevealEncounterCard first (CardWithTrait Trait.Magnetic <> CardWithType AttachmentType)
        _ -> pure ()
      pure e
    CardSpecific (isTarget attrs -> True) "mgChooseSenatorDamage" _ -> do
      first <- selectJust FirstPlayer
      senators <- selectList (AllyIs Allies.mgRobertKelly)
      chooseOnePrompted first "$label.mgChooseSenatorDamage"
        [labeled (countLabel "mgStartingSenatorDamage" n) (for_ senators (\aid -> allyStep aid (AllySetDamage n))) | n <- [0 .. 8]]
      pure e
    CardSpecific (isTarget attrs -> True) "mgChooseCapturedCards" _ -> do
      first <- selectJust FirstPlayer
      players <- getPlayerCount
      chooseOnePrompted first "$label.mgChooseCapturedCards"
        [labeled (countLabel "mgStartingCapturedCards" n) (cardSpecific attrs "mgPlaceInitialCaptured" n) | n <- [0 .. fromIntegral players + 2]]
      pure e
    CardSpecific (isTarget attrs -> True) "mgPlaceInitialCaptured" (fromJSON -> Success (n :: Natural)) -> do
      when (n > 0) $ do
        first <- selectJust FirstPlayer
        players <- filterM (fmap (not . null) . projectMap PlayerIdentityDeck unDeck) =<< getPlayers
        zero <- selectList (SideSchemeIs Sides.mgOperationZeroTolerance)
        unless (null players || null zero) $ chooseOne first
          [targeting player do
            for_ zero (\z -> putTopCardsUnder player z 1)
            cardSpecific attrs "mgPlaceInitialCaptured" (n - 1)
          | player <- players]
      pure e
    CardSpecific (isTarget attrs -> True) "revealSentinels" _ -> e <$ revealSentinels
    MainSchemeMessage mid (MainSchemePlaceThreat n) | mid == mainSchemeId attrs, toCardCode attrs == "32087" -> do
      let total = mainSchemeThreat attrs + n
      players <- getPlayerCount
      let threshold = 5 * fromIntegral players
      if total >= threshold && threshold > 0
        then do
          first <- selectJust FirstPlayer
          zero <- selectList (SideSchemeIs Sides.mgOperationZeroTolerance)
          for_ zero (\z -> putTopCardsUnder first z 1)
          cardSpecific attrs "checkThreatThreshold" ()
          pure $ MutantGenesisMainScheme attrs {mainSchemeThreat = total - threshold}
        else pure $ MutantGenesisMainScheme attrs {mainSchemeThreat = total}
    CardSpecific (isTarget attrs -> True) "checkThreatThreshold" _ -> do
      mainSchemeStep (mainSchemeId attrs) (MainSchemePlaceThreat 0)
      pure e
    PlaceCounters target n | target == toRef attrs, toCardCode attrs `elem` ["32141", "32142", "32143"] -> do
      let total = mainSchemeCounters attrs + n
      when (total >= 3) $ cardSpecific attrs "magneticReveal" ()
      pure $ MutantGenesisMainScheme attrs {mainSchemeCounters = total}
    CardSpecific (isTarget attrs -> True) "magneticReveal" _ | mainSchemeCounters attrs >= 3 -> do
      -- RR v1.8 errata: remove the counters BEFORE revealing the Magnetic card.
      discardUntil FromEncounterDeck (CardWithTrait Trait.Magnetic) attrs
      pure $ MutantGenesisMainScheme attrs {mainSchemeCounters = mainSchemeCounters attrs - 3}
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      first <- selectJust FirstPlayer
      revealEncounterCard first card
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId attrs -> do
      case toCardCode attrs of
        "32063" -> pure () -- no target threat on this stage; only Find the Senator advances it
        "32064" -> gameOver Lost
        "32087" -> pure () -- the threshold loop replaces completion
        "32112" -> nextMainScheme mid
        "32113" -> gameOver Lost
        code | code `elem` ["32126", "32127", "32128", "32129"] -> do
          victory <- getVictoryDisplay
          let completed = length [c | c <- victory, cdCardType (getCardDef c) == MainSchemeType]
          addToVictoryDisplay attrs
          if completed >= 2 then gameOver Lost else nextMainScheme mid
        "32141" -> nextMainScheme mid
        "32142" -> nextMainScheme mid
        "32143" -> gameOver Lost
        _ -> nextMainScheme mid
      pure e
    _ -> MutantGenesisMainScheme <$> runMessage msg attrs
   where
    revealSentinels = getPlayers >>= traverse_ (\p -> discardUntilIntoPlay p (CardWithType MinionType <> CardWithTrait Trait.Sentinel))
