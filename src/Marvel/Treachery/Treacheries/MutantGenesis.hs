module Marvel.Treachery.Treacheries.MutantGenesis where

import Marvel.Question (Choice)
import Marvel.Id
import Data.List (find)
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue ()
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Name (Name (..))
import Marvel.Query
import Marvel.Projection
import Marvel.Damage
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Hand
import Marvel.Card.PlayerCard.Types ()
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.Attachment.Cards qualified as Attachments ()
import Marvel.Ally.Cards qualified as Allies
import Marvel.MainScheme.Types ()
import Marvel.Cost ()
import Marvel.Keyword ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Treachery.Cards qualified as Cards

newtype MutantGenesisTreachery = MutantGenesisTreachery (Attrs Treachery `With` Maybe (IdentityId, MinionId, Bool))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery MutantGenesisTreachery where
  toTreacheryAttrs (MutantGenesisTreachery (a `With` _)) = a

mkMutantGenesisTreachery :: Attrs Treachery -> MutantGenesisTreachery
mkMutantGenesisTreachery a = MutantGenesisTreachery (a `With` Nothing)

mgSabretoothStrikes :: TreacheryCard MutantGenesisTreachery
mgSabretoothStrikes = treachery mkMutantGenesisTreachery Cards.mgSabretoothStrikes

mgUnrelentingSavage :: TreacheryCard MutantGenesisTreachery
mgUnrelentingSavage = treachery mkMutantGenesisTreachery Cards.mgUnrelentingSavage

mgMutantTerrorists :: TreacheryCard MutantGenesisTreachery
mgMutantTerrorists = treachery mkMutantGenesisTreachery Cards.mgMutantTerrorists

mgInfiltration :: TreacheryCard MutantGenesisTreachery
mgInfiltration = treachery mkMutantGenesisTreachery Cards.mgInfiltration

mgShapeshifterSurprise :: TreacheryCard MutantGenesisTreachery
mgShapeshifterSurprise = treachery mkMutantGenesisTreachery Cards.mgShapeshifterSurprise

mgSelfRepair :: TreacheryCard MutantGenesisTreachery
mgSelfRepair = treachery mkMutantGenesisTreachery Cards.mgSelfRepair

mgMutantDetected :: TreacheryCard MutantGenesisTreachery
mgMutantDetected = treachery mkMutantGenesisTreachery Cards.mgMutantDetected

mgMasterMoldsChildren :: TreacheryCard MutantGenesisTreachery
mgMasterMoldsChildren = treachery mkMutantGenesisTreachery Cards.mgMasterMoldsChildren

mgShieldsUp :: TreacheryCard MutantGenesisTreachery
mgShieldsUp = treachery mkMutantGenesisTreachery Cards.mgShieldsUp

mgBrotherhoodBeatdown :: TreacheryCard MutantGenesisTreachery
mgBrotherhoodBeatdown = treachery mkMutantGenesisTreachery Cards.mgBrotherhoodBeatdown

mgGroundSwell :: TreacheryCard MutantGenesisTreachery
mgGroundSwell = treachery mkMutantGenesisTreachery Cards.mgGroundSwell

mgImmovable :: TreacheryCard MutantGenesisTreachery
mgImmovable = treachery mkMutantGenesisTreachery Cards.mgImmovable

mgPyromaniac :: TreacheryCard MutantGenesisTreachery
mgPyromaniac = treachery mkMutantGenesisTreachery Cards.mgPyromaniac

mgHoppingMad :: TreacheryCard MutantGenesisTreachery
mgHoppingMad = treachery mkMutantGenesisTreachery Cards.mgHoppingMad

mgMasterOfMagnetism :: TreacheryCard MutantGenesisTreachery
mgMasterOfMagnetism = treachery mkMutantGenesisTreachery Cards.mgMasterOfMagnetism

mgElectricShock :: TreacheryCard MutantGenesisTreachery
mgElectricShock = treachery mkMutantGenesisTreachery Cards.mgElectricShock

mgElectromagneticBlast :: TreacheryCard MutantGenesisTreachery
mgElectromagneticBlast = treachery mkMutantGenesisTreachery Cards.mgElectromagneticBlast

mgMetalShards :: TreacheryCard MutantGenesisTreachery
mgMetalShards = treachery mkMutantGenesisTreachery Cards.mgMetalShards

mgMagneticMissile :: TreacheryCard MutantGenesisTreachery
mgMagneticMissile = treachery mkMutantGenesisTreachery Cards.mgMagneticMissile

mgZealForTheCause :: TreacheryCard MutantGenesisTreachery
mgZealForTheCause = treachery mkMutantGenesisTreachery Cards.mgZealForTheCause

instance RunMessage MutantGenesisTreachery where
  runMessage msg e@(MutantGenesisTreachery (attrs `With` missile)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      case toCardCode attrs of
        "32069" -> do
          heroes <- selectList (IdentityWithId you <> HeroIdentity <> UnexhaustedIdentity)
          senators <- selectList (AllyIs Allies.mgRobertKelly)
          chooseOrNothing you $
            [labeled "$label.mgDamageSenator" (for_ senators (\a -> damageCharacter a attrs $ toDamage 1 FromTreachery))]
            <> [labeled "$label.mgExhaustHeroPrevent" (exhaust p) | p <- heroes]
        "32070" -> do
          villains <- maybeToList <$> villainNamed "Sabretooth"
          hero <- selectAny (IdentityWithId you <> HeroIdentity)
          for_ villains \v -> do
            undamaged <- not <$> selectAny (VillainWithId v <> VillainWithAnyDamage)
            when undamaged (activationBonus v 1)
            if hero then enemyAttacks v you else villainSchemesAgainst v you
        "32078" -> do
          present <- selectAny (SideSchemeIs Sides.mgTheBrotherhood)
          candidates <- filter (cardMatch $ CardIs Sides.mgTheBrotherhood) <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
          if present || null candidates
            then discardUntil FromEncounterDeck (CardWithType MinionType <> CardWithTrait Trait.BrotherhoodOfMutants) attrs
            else searchAndRevealEncounterCard you (CardIs Sides.mgTheBrotherhood)
        "32082" -> shuffleSelf you
        "32083" -> shuffleSelf you
        "32097" -> withTheVillain you $ \v -> do
            removeStatus v StunnedStatus
            removeStatus v StunnedStatus
            removeStatus v ConfusedStatus
            removeStatus v ConfusedStatus
            removeStatus v ToughStatus
            giveStatus v ToughStatus
            heal v 5
        "32098" -> do
          zero <- selectList (SideSchemeIs Sides.mgOperationZeroTolerance)
          chooseOne you
            [labeled "$label.mgCaptureTopCard" (for_ zero (\z -> putTopCardsUnder you z 1))
            ,labeled "$label.mgAllEnemiesAttack" (attackAll you)]
        "32117" -> do
          minions <- selectList (MinionEngagedWith $ IdentityWithId you)
          hero <- selectAny (IdentityWithId you <> HeroIdentity)
          if null minions
            then villainNamed "Master Mold" >>= traverse_ (\v -> if hero then enemyAttacks v you else villainSchemesAgainst v you)
            else chooseOneAtATimePrompted you "$label.chooseMinionToActivate" [targeting (MinionRef m) (if hero then enemyAttacks m you else minionSchemes m) | m <- minions]
        "32118" -> do
          minions <- selectList (MinionWithTrait Trait.Sentinel <> MinionEngagedWith (IdentityWithId you))
          canGive <- filterM (fmap not . characterMatches (CharacterWithStatus ToughStatus) . toCharacter) minions
          if null canGive then surge you else for_ canGive (\m -> giveStatus m ToughStatus)
        "32131" -> do
          names <- traverse enemyTitle =<< selectList AnyEnemy
          when ("Avalanche" `elem` names) $ exhaust you
          when ("Blob" `elem` names) $ giveStatus you StunnedStatus
          when ("Pyro" `elem` names) $ dealIndirectDamage you attrs 2
          when ("Toad" `elem` names) $ discardFrom you RandomFromHand 1
        "32132" -> activateNamed you "Avalanche" Minions.mgAvalanche
        "32133" -> activateNamed you "Blob" Minions.mgBlob
        "32134" -> activateNamed you "Pyro" Minions.mgPyro
        "32135" -> activateNamed you "Toad" Minions.mgToad
        "32151" -> do
          discard <- getEncounterDiscard
          for_ (find (cardMatch $ CardWithTrait Trait.Magnetic) discard) \c -> do
            cardSpecific GameRef "mgGiveDiscardedBoost" c
          villainNamed "Magneto" >>= traverse_ (`villainActivatesAgainst` you)
        "32152" -> do
          hero <- selectAny (IdentityWithId you <> HeroIdentity)
          counts <- selectList EncounterMainScheme >>= traverse (\case SchemeMainSchemeId mid -> getCountersOn mid; _ -> pure 0)
          if hero
            then giveStatus you StunnedStatus >> damageIdentity you attrs (toDamage (sum counts) FromTreachery)
            else giveStatus you ConfusedStatus >> placeThreatOnEach attrs (sum counts) EncounterMainScheme
        "32153" -> do
          selectList (UpgradeControlledBy $ IdentityWithId you) >>= traverse_ exhaust
          selectList (SupportControlledBy $ IdentityWithId you) >>= traverse_ exhaust
          addMagnet
        "32154" -> do
          damageAllCharacters attrs (toDamage 1 FromTreachery) (CharacterControlledBy $ IdentityWithId you)
          addMagnet
        "32155" -> do
          minions <- selectList (MinionWithTrait Trait.Sentinel)
          chooseOrNothing you [targeting m do
            cardSpecific attrs "prepareMissile" (you, m) | m <- minions]
        "32164" -> do
          acolytes <- selectList (MinionEngagedWith (IdentityWithId you) <> MinionWithTrait Trait.Acolyte)
          if null acolytes
            then discardUntil FromEncounterDeck (CardWithType MinionType) attrs
            else chooseOneAtATimePrompted you "$label.mgChooseAcolyteOrder" [targeting (MinionRef m) (cardSpecific m "mgResolveDefeat" ()) | m <- acolytes]
        _ -> pure ()
      pure $ MutantGenesisTreachery (attrs {treacheryResolver = Just you} `With` missile)
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard c) -> do
      you <- maybe getYou pure (treacheryResolver attrs)
      revealEncounterCard you c
      pure e
    CardSpecific (isTarget attrs -> True) "mgOrderDetectedMinions" (fromJSON -> Success you) -> do
      minions <- selectList (MinionEngagedWith $ IdentityWithId you)
      unless (null minions) $ chooseOneAtATimePrompted you "$label.chooseMinionToActivate"
        [targeting (MinionRef mid) (enemyAttacks mid you) | mid <- minions]
      pure e
    CardSpecific (isTarget attrs -> True) "prepareMissile" (fromJSON -> Success (you, mid)) -> do
      defeatMinion mid
      cardSpecific attrs "missileDamage" (you, mid)
      pure $ MutantGenesisTreachery (attrs `With` Just (you, mid, False))
    DefeatedLeavesPlay (MinionRef mid) | Just (you, target, _) <- missile, mid == target -> pure $ MutantGenesisTreachery (attrs `With` Just (you, target, True))
    CardSpecific (isTarget attrs -> True) "missileDamage" (fromJSON -> Success (you, mid)) -> do
      present <- selectAny (MinionWithId mid)
      cards <- projectMap PlayerIdentityHand unHand you
      when (not present && maybe False (\(_, target, defeated) -> target == mid && defeated) missile) $ chooseOne you [labeled (countLabel "mgDiscardPreventDamage" n) do
        discardFor you attrs FromHand n n
        damageIdentity you attrs (toDamage (5 - min 5 n) FromTreachery)
        | n <- [0 .. min 5 (fromIntegral $ length cards)]]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "32097" -> withTheVillain you (\v -> giveStatus v ToughStatus)
        "32118" -> withTheVillain you (\v -> giveStatus v ToughStatus)
        "32153" -> exhaust you
        "32132" -> extraBoost "Avalanche"
        "32133" -> extraBoost "Blob"
        "32134" -> extraBoost "Pyro"
        "32135" -> extraBoost "Toad"
        "32069" -> cardSpecific GameRef "mgAfterBoostAllyDefeatThreat" (enemy, 2 :: Natural)
        "32154" -> cardSpecific GameRef "mgAfterBoostAllyDefeatMagnet" enemy
        _ -> pure ()
      pure e
    _ -> MutantGenesisTreachery . (`With` missile) <$> runMessage msg attrs
   where
    shuffleSelf you = do
      let card = MkEncounterCard (CardId $ unTreacheryId $ treacheryId attrs) (getCardDef attrs)
      removeFromGame attrs
      shuffleIntoDeck you [encounterCardInPlayerDeck you card]
      surge you
    attackAll you = withTheVillainIfAny you $ \villain -> do
      -- RR v1.8 Activation: the villain resolves first, then the player orders minions.
      for_ villain (`enemyAttacks` you)
      cardSpecific attrs "mgOrderDetectedMinions" you
    addMagnet = selectList EncounterMainScheme >>= traverse_ (\case SchemeMainSchemeId mid -> placeCounters mid 1; SchemeSideSchemeId sid -> placeCounters sid 1)
    enemyTitle enemy = fmap (maybe "" (title . cdName)) $ getsGame (`abilityCardDef` enemyRef enemy)
    activateNamed you name def = do
      enemies <- filterM (fmap (== name) . enemyTitle) =<< selectList AnyEnemy
      if null enemies then searchAndRevealEncounterCard you (CardIs def)
        else for_ enemies \case
          EnemyVillainId v -> villainActivatesAgainst v you
          EnemyMinionId m -> do
            hero <- selectAny (IdentityWithId you <> HeroIdentity)
            if hero then enemyAttacks m you else minionSchemes m
    extraBoost name = do
      vs <- filterM (fmap (== name) . enemyTitle . EnemyVillainId) =<< selectList AnyVillain
      traverse_ dealBoost vs

chooseOrNothing :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
chooseOrNothing you choices = unless (null choices) $ chooseOrRunOne you choices
