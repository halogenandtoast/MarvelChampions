module Marvel.Encounter.CampaignNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.CampaignPlayers.EncounterCards qualified as C
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Modifier
import Marvel.Matchers
import Marvel.Minion.Runner qualified as Minion
import Marvel.Minion.Cards qualified as M
import Marvel.Obligation ()
import Marvel.Obligation.Types qualified as O
import Marvel.Query
import Marvel.SideScheme.Types qualified as Scheme
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "Civil War and Fear No Evil nemeses" do
  for_ [(E.TigraNemesis,5),(E.HulklingNemesis,5),(E.DaredevilNemesis,5),(E.EchoNemesis,5)] $ \(set,n) ->
    it ("registers every physical card in " <> show set) do
      tg <- testGame ["spider-man"]
      inGame tg $ length <$> gatherEncounterSet set >>= (`shouldBe` n)

  it "protects Criminal minions while Criminal Underworld is in play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) C.enc56024
      _ <- revealNow (player1 tg) C.enc56025
      resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 2 FromAbility)]
      minionDamageOf mid `shouldReturn` Just 0

  it "reveals Criminal Underworld when Madame Masque enters play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [C.enc56025,M.hydraMercenary,M.hydraSoldier] []
      _ <- revealFully (player1 tg) C.enc56027
      selectListCount (SideSchemeIs C.enc56025) `shouldReturn` 1

  it "adds threat to Criminal Underworld when Madame Masque finds it already in play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) C.enc56025
      _ <- revealFully (player1 tg) C.enc56027
      sid <- selectJust $ SideSchemeIs C.enc56025
      schemes <- getsGame gameSideSchemes
      liftIO $ Scheme.getSideSchemeThreat (schemes HashMap.! sid) `shouldBe` 6

  it "lets Henchmen protect Kingpin until the final Henchman leaves play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      kingpin <- addMinion (player1 tg) C.enc60061
      henchman <- addMinion (player1 tg) C.enc60063
      resolve [MinionMessage kingpin (MinionDamaged GameRef $ toDamage 2 FromAbility)]
      minionDamageOf kingpin `shouldReturn` Just 0
      resolve [MinionMessage henchman (MinionDamaged GameRef $ toDamage 4 FromAbility)]
      resolve [MinionMessage kingpin (MinionDamaged GameRef $ toDamage 2 FromAbility)]
      minionDamageOf kingpin `shouldReturn` Just 2

  it "protects Kingpin with Master Manipulator and starts with two threat per player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      _ <- setEncounterDeck [C.enc60061,M.hydraMercenary,M.hydraSoldier] []
      _ <- revealFully (player1 tg) C.enc60062
      kingpin <- selectJust $ MinionIs C.enc60061
      resolve [MinionMessage kingpin (MinionDamaged GameRef $ toDamage 2 FromAbility)]
      minionDamageOf kingpin `shouldReturn` Just 0
      sid <- selectJust $ SideSchemeIs C.enc60062
      schemes <- getsGame gameSideSchemes
      liftIO $ Scheme.getSideSchemeThreat (schemes HashMap.! sid) `shouldBe` 4

  it "attaches Fantastic Powers to Super Skrull, increasing both stats and hit points" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck [C.enc56055,M.hydraMercenary,M.hydraSoldier] []
      _ <- revealFully (player1 tg) C.enc56057
      mid <- selectJust $ MinionIs C.enc56055
      minions <- getsGame gameMinions
      let attrs = toAttrs (minions HashMap.! mid)
      Minion.getModifiedHitPoints attrs `shouldReturn` 8
      Minion.getModifiedAttack attrs `shouldReturn` 2
      Minion.getModifiedScheme attrs `shouldReturn` 2

  it "discards facedown player cards under Skrull Business when it leaves play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p C.enc56056
      sid <- selectJust $ SideSchemeIs C.enc56056
      card <- putInHand p "01089"
      resolve [DiscardedCard $ PlayerCard card]
      start [CheckWindows [W.Window W.After $ W.DiscardedFromPlayerDeck p [card]]]
      choose "resolve Skrull Business" (isAbilityOf sid)
      schemes <- getsGame gameSideSchemes
      liftIO $ map pcCardId (Scheme.sideSchemeHeldCards $ toAttrs $ schemes HashMap.! sid) `shouldBe` [pcCardId card]
      resolve [RemoveFromPlay $ SideSchemeRef sid]
      discarded <- unDiscard . playerIdentityDiscard <$> playerAttrs p
      liftIO $ map pcCardId discarded `shouldSatisfy` elem (pcCardId card)

  it "lets Bullseye attack an alter-ego when no Persona support is discarded" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- revealFully (player1 tg) C.enc60033
      pass
      damageOf (player1 tg) `shouldReturn` 3

  it "places Jigsaw's two threat after his attack damages a character" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p C.enc56025
      mid <- addMinion p C.enc56028
      start [MinionMessage mid $ MinionAttacks p]
      pass
      choose "resolve Jigsaw" (isAbilityOf mid)
      pass
      sid <- selectJust $ SideSchemeIs C.enc56025
      schemes <- getsGame gameSideSchemes
      liftIO $ Scheme.getSideSchemeThreat (schemes HashMap.! sid) `shouldBe` 6

  it "makes Kingpin scheme instead of attacking Echo" do
    tg <- testGame ["echo"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      mid <- addMinion p C.enc60061
      before <- mainSchemeThreat
      start [MinionMessage mid $ MinionAttacks p]
      choose "resolve Kingpin" (isAbilityOf mid)
      pass
      mainSchemeThreat `shouldReturn` before + 3
      damageOf p `shouldReturn` 0

  it "keeps Sensory Overload in play and discards it after recovering" do
    tg <- testGame ["daredevil"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p C.enc60032
      obligations <- getsGame gameObligations
      let oid = testHead $ HashMap.keys obligations
      setDamage p 3
      start [CheckWindows [W.Window W.After $ W.UsedRecovery p]]
      choose "discard Sensory Overload" (isAbilityOf (ObligationRef oid :: Target))
      pass
      obligationIsInPlay oid `shouldReturn` False

  it "prevents the identity from damaging Kingpin but still allows its ally to do so" do
    tg <- testGame ["echo"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [C.enc60061,M.hydraMercenary,M.hydraSoldier] []
      _ <- revealFully p C.enc60060
      kingpin <- selectJust $ MinionIs C.enc60061
      resolve [MinionMessage kingpin (MinionDamaged (IdentityRef p) $ toDamage 2 FromAbility)]
      minionDamageOf kingpin `shouldReturn` Just 0
      ally <- putIntoPlay p "01084"
      resolve [MinionMessage kingpin (MinionDamaged (AllyRef $ AllyId $ unCardId $ pcCardId ally) $ toDamage 2 FromAbility)]
      minionDamageOf kingpin `shouldReturn` Just 2

  it "removes Raised by the Kingpin after four threat is removed by its owner's thwarts" do
    tg <- testGame ["echo"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [C.enc60061,M.hydraMercenary,M.hydraSoldier] []
      _ <- revealFully p C.enc60060
      obligations <- getsGame gameObligations
      let oid = testHead $ HashMap.keys obligations
      sid <- mainSchemeId
      start [CheckWindows [W.Window W.After $ W.ThwartRemovedThreat (IdentityRef p) (SchemeMainSchemeId sid) 5 4]]
      choose "place removed threat on Raised by the Kingpin" (isAbilityOf (ObligationRef oid :: Target))
      pass
      obligationIsInPlay oid `shouldReturn` False

  it "counts distinct printed resource types across You're Coming With Me's discarded cards" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      cards <- traverse (putInHand p) ["01088","01089","01090"]
      resolve [IdentityMessage p $ PlaceOnTopOfDeck card | card <- cards]
      before <- mainSchemeThreat
      _ <- revealFully p C.enc56058
      pass
      mainSchemeThreat `shouldReturn` before + 3

  it "uses the chosen hero's ATK for Pawn of the Kingpin's non-attack damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      _ <- revealFully p C.enc60064
      pass
      damageOf p `shouldReturn` 2

  it "reveals Bullseye through Eye on the Target when no sacrifice is chosen" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [C.enc60033,M.hydraMercenary,M.hydraSoldier] []
      _ <- revealFully p C.enc60036
      pass
      selectListCount (MinionIs C.enc60033) `shouldReturn` 1
      damageOf p `shouldReturn` 3

  it "attaches The Hood's Mantle to the Hood before falling back to the villain" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p C.enc56024
      _ <- revealNow p C.enc56026
      minions <- getsGame gameMinions
      let attrs = toAttrs (minions HashMap.! mid)
      Minion.getModifiedAttack attrs `shouldReturn` 2
      Minion.getModifiedScheme attrs `shouldReturn` 2

  it "exhausts a ready alter-ego to remove In Too Deep from the game" do
    tg <- testGame ["tigra"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p C.enc56023
      choose "exhaust and remove the obligation" (isLabel "$label.cnExhaustRemoveObligation")
      pass
      isExhaustedPlayer p `shouldReturn` True
      encounterDiscardCodes >>= (`shouldSatisfy` notElem "56023")

  it "discards Hulkling's Shapeshift upgrades for Complicated Lineage's second option" do
    tg <- testGame ["hulkling"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "56032"
      _ <- revealNow p C.enc56054
      choose "discard Shapeshift upgrades" (isLabel "$label.cnDiscardShapeshifts")
      pass
      upgrades <- getsGame gameUpgrades
      liftIO $ HashMap.member (UpgradeId $ unCardId $ pcCardId card) upgrades `shouldBe` False

  it "gives the enemy with the highest ATK Stolen Sai's attack bonus and piercing" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) C.enc60033
      _ <- revealFully (player1 tg) C.enc60035
      minions <- getsGame gameMinions
      Minion.getModifiedAttack (toAttrs $ minions HashMap.! mid) `shouldReturn` 4
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem (KeywordModifier Piercing))

  it "gives attacking Bullseye a boost card and applies Deadliest Man Alive's amplification" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p C.enc60034
      mid <- addMinion p C.enc60033
      setDamage p 0
      sid <- selectJust $ SideSchemeIs C.enc60034
      _ <- setEncounterDeck [C.enc56023,C.enc56023] []
      start [MinionMessage mid $ MinionAttacks p]
      choose "give Bullseye a boost" (isAbilityOf sid)
      pass
      damageOf p `shouldReturn` 6

  it "adds two damage to Super Skrull's attack when it discards a printed energy resource" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p C.enc56055
      _ <- stackDeck p ["01088"]
      start [MinionMessage mid $ MinionAttacks p]
      choose "resolve Super Skrull's resource" (isAbilityOf mid)
      pass
      damageOf p `shouldReturn` 3
      discarded <- discardOf p
      liftIO $ map toCardCode discarded `shouldSatisfy` elem "01088"

  it "deals Sensory Overload's damage when a Sense upgrade enters play" do
    tg <- testGame ["daredevil"]
    let p = player1 tg
    inGame tg do
      _ <- revealNow p C.enc60032
      _ <- putIntoPlay p "60002"
      damageOf p `shouldReturn` 1

  it "preserves an obligation's owner and accumulated threat when saving and loading" do
    tg <- testGame ["echo"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [C.enc60061,M.hydraMercenary,M.hydraSoldier] []
      _ <- revealFully p C.enc60060
      sid <- mainSchemeId
      start [CheckWindows [W.Window W.After $ W.ThwartRemovedThreat (IdentityRef p) (SchemeMainSchemeId sid) 5 3]]
      choose "place three removed threat on Raised by the Kingpin" isAnyAbility
      pass
      obligation <- testHead . HashMap.elems <$> getsGame gameObligations
      case fromJSON (toJSON obligation) :: Result O.Obligation of
        Error err -> liftIO $ expectationFailure err
        Success restored -> do
          liftIO $ O.getObligationDisplayThreat restored `shouldBe` 3
          liftIO $ O.obligationOwner (toAttrs restored) `shouldBe` Just p

  it "lets the player order In Too Deep's activations and surges if every activation is prevented" do
    tg <- testGame ["tigra"]
    let p = player1 tg
    inGame tg do
      first <- addMinion p M.hydraMercenary
      second <- addMinion p M.hydraMercenary
      resolve [GiveStatus (MinionRef mid) ConfusedStatus | mid <- [first,second]]
      _ <- setEncounterDeck [M.hydraSoldier,M.hydraMercenary] []
      before <- length . playerIdentityEncounterCards <$> playerAttrs p
      _ <- revealNow p C.enc56023
      choose "activate engaged minions" (isLabel "$label.cnActivateMinions")
      offers (isTargetChoice first) `shouldReturn` True
      offers (isTargetChoice second) `shouldReturn` True
      choose "activate the second minion first" (isTargetChoice second)
      choose "activate the remaining minion" (isTargetChoice first)
      pass
      length . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` before + 1

  it "does not surge when an In Too Deep minion actually activates" do
    tg <- testGame ["tigra"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p M.hydraMercenary
      before <- length . playerIdentityEncounterCards <$> playerAttrs p
      _ <- revealNow p C.enc56023
      choose "activate engaged minions" (isLabel "$label.cnActivateMinions")
      choose "activate the minion" (isTargetChoice mid)
      pass
      length . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` before

revealFully :: IdentityId -> CardDef -> GameAppT EncounterCard
revealFully you def = do
  card <- encounterCard def
  resolve [RevealEncounterCard you card]
  pure card
