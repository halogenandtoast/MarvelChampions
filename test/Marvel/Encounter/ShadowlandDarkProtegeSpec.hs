module Marvel.Encounter.ShadowlandDarkProtegeSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as FNE
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Shadowland.Cards qualified as Cards
import TestImport

spec :: Spec
spec = describe "Shadowland Dark Protege linked cards" do
  it "Art of the Kill increases the attack and places threat if that attack defeats a defending ally" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      ally <- putIntoPlay you "01002"
      card <- revealNow you Cards.street65018
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      vid <- villainIdNow
      _ <- setEncounterDeck (replicate 3 FNE.street60188) []
      runNow [VillainMessage vid $ VillainAttacks you]
      choose "increase the attack" (isAbilityOf aid)
      choose "defend with Black Cat" (\case AllyDefend who _ -> who == allyIdOf ally; _ -> False)
      choose "resolve the defeated character penalty" (isAbilityOf aid)
      pass
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` 1

  it "Archpriestess discards three encounter cards if she has no other minion to attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      (cards,_) <- setEncounterDeck (replicate 4 FNE.street60188) []
      _ <- addMinion you Cards.street65020
      getEncounterDeck `shouldReturn` drop 3 cards

  for_ [False,True] $ \stunned ->
    it ("Archpriestess tracks whether another minion actually attacked, stunned=" <> show stunned) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        clearHand you
        mid <- addMinion you FNE.street60092
        when stunned $ resolve [GiveStatus (MinionRef mid) StunnedStatus]
        (cards,_) <- setEncounterDeck (replicate 4 FNE.street60188) []
        _ <- addMinion you Cards.street65020
        damageOf you `shouldReturn` (if stunned then 0 else 2)
        getEncounterDeck `shouldReturn` (if stunned then drop 3 cards else cards)

  it "Gorgon stuns the attacking hero after taking attack damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      mid <- addMinion you Cards.street65021
      choose "attack Gorgon" (isHeroAttack you)
      choose "target Gorgon" (isTargetChoice mid)
      choose "resolve Gorgon's response" (isAbilityOf mid)
      pass
      minionDamageOf mid `shouldReturn` Just 2
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1

  it "Gorgon stuns an attacking ally rather than its controller" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01002"
      ally <- allyIn "01002"
      mid <- addMinion you Cards.street65021
      choose "attack with Black Cat" (isAllyAttack ally)
      choose "target Gorgon" (isTargetChoice mid)
      choose "stun the attacker" (isAbilityOf mid)
      pass
      getCharacterStatusCount (AllyCharacter $ allyId ally) StunnedStatus `shouldReturn` 1
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 0

  it "Lady Bullseye attacks the highest-printed-cost ally controlled by the attacked player" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay you "01002"
      _ <- putIntoPlay you "01084"
      mid <- addMinion you Cards.street65022
      _ <- setEncounterDeck (replicate 4 FNE.street60188) []
      resolve [MinionMessage mid $ MinionAttacks you]
      allyDamage <$> allyIn "01084" `shouldReturn` 2
      allyDamage <$> allyIn "01002" `shouldReturn` 0
      damageOf you `shouldReturn` 0

  it "White Tiger deals two indirect damage when revealed" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- addMinion (player1 tg) Cards.street65023
      damageOf (player1 tg) `shouldReturn` 2

  for_ [False,True] $ \hasAlly ->
    it ("Rite of Offering removes an ally or damages its defeater, ally present=" <> show hasAlly) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        ally <- if hasAlly then Just <$> putIntoPlay you "01002" else pure Nothing
        card <- revealNow you Cards.street65024
        pass
        let sid = SideSchemeId $ unCardId $ ecCardId card
        resolve =<< L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 1)
        damageOf you `shouldReturn` (if hasAlly then 0 else 3)
        for_ ally $ \removed -> do
          selectAny (AllyWithId $ allyIdOf removed) `shouldReturn` False
          discardOf you >>= \cards -> liftIO $ map pcCardId cards `shouldNotContain` [pcCardId removed]

  it "Shadowy Plot reveals one of the top three physical encounter cards and discards the other two" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- revealNow you Cards.street65025
      pass
      ([chosen,other,third,_,_],_) <- setEncounterDeck [FNE.street60092,FNE.street60085,FNE.street60094,FNE.street60188,FNE.street60188] []
      resolve =<< L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId $ SideSchemeId $ unCardId $ ecCardId card) 2)
      selectAny (MinionWithId $ MinionId $ unCardId $ ecCardId chosen) `shouldReturn` True
      discarded <- getEncounterDiscard
      liftIO $ map ecCardId discarded `shouldContain` [ecCardId other]
      liftIO $ map ecCardId discarded `shouldContain` [ecCardId third]
      liftIO $ map ecCardId discarded `shouldNotContain` [ecCardId chosen]
      length <$> getEncounterDeck `shouldReturn` 2

  for_ [("01094",1,1),("60160a",3,4)] $ \(villain,threat,remaining) ->
    it ("Embraced by Darkness checks the actual threat placed by " <> show villain) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        vid <- villainIdNow
        when (villain /= "01094") $ resolve [ReplaceVillain vid villain Advanced]
        _ <- setEncounterDeck (replicate 5 FNE.street60188) []
        card <- encounterCard Cards.street65027
        resolve [RevealEncounterCard you card]
        mainSchemeThreatNow `shouldReturn` threat
        length <$> getEncounterDeck `shouldReturn` remaining

  it "Art of the Kill can be discarded by paying its three different resources in alter-ego form" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      clearHand you
      card <- revealNow you Cards.street65018
      pass
      resources <- traverse (putInHand you) ["01088","01089","01090"]
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "discard Art of the Kill" (isAbilityOf aid)
      payWith resources
      pass
      selectAny (AttachmentWithId aid) `shouldReturn` False

  it "Krahllak's Blessing reduces character damage and attacks that character before discarding" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      card <- revealNow you Cards.street65019
      pass
      _ <- setEncounterDeck (replicate 3 FNE.street60188) []
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) $ toDamage 4 $ FromPlayerAttack you]
      getVillainHitPoints vid `shouldReturn` 13
      damageOf you `shouldReturn` 2
      selectAny (AttachmentWithId $ AttachmentId $ unCardId $ ecCardId card) `shouldReturn` False

  it "Krahllak's Blessing does not treat damage from a support as damage from its controller's identity" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      team <- putIntoPlay you "01056"
      card <- revealNow you Cards.street65019
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      choose "use Tactical Team" (isAbilityOf $ supportIdOf team)
      choose "damage Rhino" (isTargetChoice vid)
      offers (isAbilityOf aid) `shouldReturn` False
      pass
      getVillainHitPoints vid `shouldReturn` 12
      damageOf you `shouldReturn` 0
      selectAny (AttachmentWithId aid) `shouldReturn` True
