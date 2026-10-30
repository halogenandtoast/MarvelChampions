module Marvel.SideScheme.SideSchemes.MutantGenesis where

import Marvel.Id
import Marvel.Prelude
import Marvel.Entity
import Marvel.Card
import Marvel.Card.PlayerCard.Types (encounterCardInPlayerDeck)
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Query
import Marvel.Projection
import Marvel.Modifier
import Marvel.Keyword
import Marvel.Damage
import Marvel.Trait qualified as Trait
import Marvel.Ally.Cards qualified as Allies
import Marvel.Minion.Cards qualified as Minions
import Marvel.Environment.Cards qualified as Environments
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Cost
import Marvel.Question
import System.Random.Shuffle (shuffleM)
import Marvel.SideScheme.Cards qualified as Cards

newtype MutantGenesisSideScheme = MutantGenesisSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mgFindTheSenator :: SideSchemeCard MutantGenesisSideScheme
mgFindTheSenator = sideSchemeWith MutantGenesisSideScheme Cards.mgFindTheSenator (PerPlayer 5) (crisisL .~ True)

mgMedicalEmergency :: SideSchemeCard MutantGenesisSideScheme
mgMedicalEmergency = sideScheme MutantGenesisSideScheme Cards.mgMedicalEmergency (Static 2)

mgFeralRage :: SideSchemeCard MutantGenesisSideScheme
mgFeralRage = sideSchemeWith MutantGenesisSideScheme Cards.mgFeralRage (Static 4) (crisisL .~ True)

mgTheBrotherhood :: SideSchemeCard MutantGenesisSideScheme
mgTheBrotherhood = sideScheme MutantGenesisSideScheme Cards.mgTheBrotherhood (Static 3)

mgMetamorphicMayhem :: SideSchemeCard MutantGenesisSideScheme
mgMetamorphicMayhem = sideScheme MutantGenesisSideScheme Cards.mgMetamorphicMayhem (Static 3)

mgMutantsAtTheMall :: SideSchemeCard MutantGenesisSideScheme
mgMutantsAtTheMall = sideScheme MutantGenesisSideScheme Cards.mgMutantsAtTheMall (Static 4)

mgAbductionProtocols :: SideSchemeCard MutantGenesisSideScheme
mgAbductionProtocols = sideSchemeWith MutantGenesisSideScheme Cards.mgAbductionProtocols (Static 2) (crisisL .~ True)

mgOperationZeroTolerance :: SideSchemeCard MutantGenesisSideScheme
mgOperationZeroTolerance = sideScheme MutantGenesisSideScheme Cards.mgOperationZeroTolerance (PerPlayer 3)

mgRelentlessRobots :: SideSchemeCard MutantGenesisSideScheme
mgRelentlessRobots = sideScheme MutantGenesisSideScheme Cards.mgRelentlessRobots (PerPlayer 3)

mgIntruderAlert :: SideSchemeCard MutantGenesisSideScheme
mgIntruderAlert = sideSchemeWith MutantGenesisSideScheme Cards.mgIntruderAlert (PerPlayer 3) (crisisL .~ True)

mgInsertVirusProgram :: SideSchemeCard MutantGenesisSideScheme
mgInsertVirusProgram = sideScheme MutantGenesisSideScheme Cards.mgInsertVirusProgram (Static 2)

mgProtectTheStudents :: SideSchemeCard MutantGenesisSideScheme
mgProtectTheStudents = sideSchemeWith MutantGenesisSideScheme Cards.mgProtectTheStudents (Static 2) (crisisL .~ True)

mgUnderSiege :: SideSchemeCard MutantGenesisSideScheme
mgUnderSiege = sideScheme MutantGenesisSideScheme Cards.mgUnderSiege (PerPlayer 0)

mgBoardingParty :: SideSchemeCard MutantGenesisSideScheme
mgBoardingParty = sideScheme MutantGenesisSideScheme Cards.mgBoardingParty (PerPlayer 3)

mgSabotageMasterMold :: SideSchemeCard MutantGenesisSideScheme
mgSabotageMasterMold = sideScheme MutantGenesisSideScheme Cards.mgSabotageMasterMold (PerPlayer 3)

mgOrbitalDecay :: SideSchemeCard MutantGenesisSideScheme
mgOrbitalDecay = sideScheme MutantGenesisSideScheme Cards.mgOrbitalDecay (PerPlayer 3)

mgMagneticMayhem :: SideSchemeCard MutantGenesisSideScheme
mgMagneticMayhem = sideScheme MutantGenesisSideScheme Cards.mgMagneticMayhem (Static 4)

mgMagneticallySealed :: SideSchemeCard MutantGenesisSideScheme
mgMagneticallySealed = sideSchemeWith MutantGenesisSideScheme Cards.mgMagneticallySealed (Static 2) (crisisL .~ True)

mgSeized :: SideSchemeCard MutantGenesisSideScheme
mgSeized = sideScheme MutantGenesisSideScheme Cards.mgSeized (PerPlayer 3)

mgTheAcolytes :: SideSchemeCard MutantGenesisSideScheme
mgTheAcolytes = sideScheme MutantGenesisSideScheme Cards.mgTheAcolytes (PerPlayer 3)

mgNimrodsPortal :: SideSchemeCard MutantGenesisSideScheme
mgNimrodsPortal = sideScheme MutantGenesisSideScheme Cards.mgNimrodsPortal (PerPlayer 3)

mgBastionsMachinations :: SideSchemeCard MutantGenesisSideScheme
mgBastionsMachinations = sideScheme MutantGenesisSideScheme Cards.mgBastionsMachinations (Static 6)

instance IsSideScheme MutantGenesisSideScheme

instance HasModifiersFor MutantGenesisSideScheme where
  getModifiersFor _ target (MutantGenesisSideScheme a) = case toCardCode a of
    "32065a" | AllyRef aid <- target -> do
      senator <- selectAny (AllyWithId aid <> AllyIs Allies.mgRobertKelly)
      pure $ if senator then [Blank, CannotHaveAttachedPlayerCards, CannotBeHealedByPlayerCards] else []
    "32108" | target == toRef a -> do
      you <- getYou
      sentinel <- selectAny (MinionEngagedWith (IdentityWithId you) <> MinionWithTrait Trait.Sentinel)
      pure [ThreatCannotBeRemovedByThwarting | sentinel]
    "32079" | MinionRef mid <- target -> do
      brotherhood <- selectAny (MinionWithId mid <> MinionWithTrait Trait.BrotherhoodOfMutants)
      pure [KeywordModifier Quickstrike | brotherhood]
    "32144a" | VillainRef _ <- target -> cap 6
    "32144b" | VillainRef _ <- target -> cap 12
    "32145a" | VillainRef _ <- target -> cap 18
    "32165" | MinionRef mid <- target -> do
      acolyte <- selectAny (MinionWithId mid <> MinionWithTrait Trait.Acolyte)
      pure [KeywordModifier Guard | acolyte]
    _ -> pure []
   where
    cap n = do
      players <- getPlayerCount
      pure [MaximumSustainedDamage $ n * fromIntegral players]

instance RunMessage MutantGenesisSideScheme where
  runMessage msg e@(MutantGenesisSideScheme attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "mgHoldCard" (fromJSON -> Success card) -> do
      let cards = sideSchemeHeldCards attrs <> [card]
      when (toCardCode attrs == "32104") $ do
        players <- getPlayerCount
        when (length cards >= players + 3) (gameOver Lost)
      pure $ MutantGenesisSideScheme attrs {sideSchemeHeldCards = cards}
    CardSpecific (isTarget attrs -> True) "mgClearHeld" _ -> pure $ MutantGenesisSideScheme attrs {sideSchemeHeldCards = []}
    CardSpecific (isTarget attrs -> True) "mgRemoveHeld" (fromJSON -> Success cid) -> pure $ MutantGenesisSideScheme attrs {sideSchemeHeldCards = filter ((/= cid) . pcCardId) (sideSchemeHeldCards attrs)}
    SideSchemeMessage sid (SideSchemeThwarted source _) | sid == sideSchemeId attrs, toCardCode attrs == "32108" -> do
      controller <- getSourceController source
      forbidden <- maybe (pure False) (\you -> selectAny (MinionEngagedWith (IdentityWithId you) <> MinionWithTrait Trait.Sentinel)) controller
      if forbidden then pure e else MutantGenesisSideScheme <$> maybe (runMessage msg attrs) (\you -> asYou you $ runMessage msg attrs) controller
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      case toCardCode attrs of
        "32137" -> do
          characters <- selectList (CharacterWithTrait Trait.BrotherhoodOfMutants)
          placeThreatOnEach attrs (3 * fromIntegral (length characters)) (SchemeWithId $ SchemeSideSchemeId sid)
        "32157" -> do
          allies <- selectList AnyAlly
          placeThreatOnEach attrs (2 * fromIntegral (length allies)) (SchemeWithId $ SchemeSideSchemeId sid)
        "32158" -> getPlayers >>= traverse_ (\you -> putTopCardsUnder you attrs 6)
        "32169" -> getPlayers >>= traverse_ (\you -> putTopCardsUnder you attrs 9)
        _ -> pure ()
      MutantGenesisSideScheme <$> runMessage msg attrs
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- maybe getYou pure (sideSchemeDefeatedBy attrs)
      first <- selectJust FirstPlayer
      case toCardCode attrs of
        "32065a" -> do
          cardSpecific GameRef "mgRescueSenator" ()
          selectList EncounterMainScheme >>= traverse_ (\case SchemeMainSchemeId mid -> nextMainScheme mid; _ -> pure ())
          flipInto Environments.mgProtectTheSenator
        "32071" -> selectList (AllyIs Allies.mgRobertKelly) >>= traverse_ (\kelly -> healFrom attrs kelly 2)
        "32072" -> villainNamed "Sabretooth" >>= traverse_ (`enemyAttacks` you)
        "32081" -> do
          discard <- filter (cardMatch $ CardWithTrait Trait.Shapeshifter) <$> getEncounterDiscard
          for_ discard (\c -> removeFromGame (CardIdRef $ ecCardId c))
          shuffleIntoDeck you (map (encounterCardInPlayerDeck you) discard)
        "32088a" -> do
          searchAndRevealEncounterCard first (CardWithType MinionType <> CardWithTrait Trait.Sentinel)
          cardSpecific GameRef "mgRescueJubilee" ()
          removeFromGame attrs
        "32100" -> do
          captives <- filter (cardMatch $ CardWithTrait Trait.Captive) <$> getSetAsidePlayerCards
          shuffled <- shuffleM captives
          for_ (headMay shuffled) (putIntoPlay you)
        "32119" -> discardUntilIntoPlay you (CardWithType MinionType <> CardWithTrait Trait.Sentinel)
        "32120" -> damageAllEnemies attrs (toDamage 2 FromAbility) (EnemyWithTrait Trait.Sentinel)
        "32136" -> search you (SearchIdentityDeckAndDiscardPile you) (CardWithType AllyType) SearchDrawOne ShuffleBackIn
        "32144a" -> flipAndReveal Cards.mgSabotageMasterMold
        "32144b" -> do
          revealEncounterCard first =<< genEncounterCard Cards.mgOrbitalDecay
          addToVictoryDisplay attrs
        "32145a" -> flipAndReveal Attachments.mgPhysicalStrain
        "32156" -> discardTopOfEncounterDeckThen 4 (\cards -> do
          let n = fromIntegral $ length $ filter (cardMatch $ CardWithTrait Trait.Magnetic) cards
          selectList EncounterMainScheme >>= traverse_ (\case SchemeMainSchemeId mid -> placeCounters mid n; SchemeSideSchemeId sid -> placeCounters sid n))
        "32158" -> villainNamed "Magneto" >>= traverse_ (`villainActivatesAgainst` you)
        "32168" -> do
          players <- getPlayers
          for_ players (\p -> discardTopOfEncounterDeckThen 2 (countBoostIcons attrs p))
        _ -> pure ()
      MutantGenesisSideScheme <$> runMessage msg attrs
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) counts -> e <$ dealIndirectDamage you attrs (sum $ map snd counts)
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "32157" -> selectList (AllyControlledBy $ IdentityWithId you) >>= traverse_ exhaust
        "32165" -> do
          cards <- filter (cardMatch $ CardWithTrait Trait.Acolyte <> CardWithType MinionType) <$> getEncounterDiscard
          shuffleIntoEncounterDeck cards
        _ -> pure ()
      pure e
    _ -> MutantGenesisSideScheme <$> runMessage msg attrs
   where
    flipAndReveal def = do
      first <- selectJust FirstPlayer
      removeFromGame attrs
      revealEncounterCard first =<< genEncounterCard def
    flipInto def = do
      first <- selectJust FirstPlayer
      removeFromGame attrs
      putEncounterCardIntoPlay first =<< genEncounterCard def

chooseOrNothing :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrNothing you choices = unless (null choices) $ chooseOrRunOne you choices
