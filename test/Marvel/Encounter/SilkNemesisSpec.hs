module Marvel.Encounter.SilkNemesisSpec (spec) where
import TestImport
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Silk.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Types qualified as Minion
import Data.HashMap.Strict qualified as HashMap

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)

perform :: L.QueueT GameAppT () -> GameAppT ()
perform body = L.runQueueT (L.capture body) >>= runNow

spec :: Spec
spec = describe "Silk nemesis and Growing Strong" do
  it "Hunting the Spider-Bride tucks itself without entering the discard pile" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Cards.huntingtheSpiderBrideSilk
      pass
      tucked <- L.tuckedEncounterCards p
      tucked `shouldSatisfy` elem card
      discarded <- getEncounterDiscard
      discarded `shouldSatisfy` notElem card

  it "discarding Hunting the Spider-Bride with Cindy's player ability deals two damage" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- encounterCard Cards.huntingtheSpiderBrideSilk
      perform (L.tuckEncounterCard p card)
      pass
      choose "Cindy Moon" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
      choose "Hunting the Spider-Bride" (isTargetChoice card)
      pass
      damageOf p `shouldReturn` 2

  it "Morlun gains one ATK and SCH for each encounter card under identities" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.morlunSilk
      perform (L.tuckTopEncounterCard p)
      pass
      attrs <- minionAttrs mid
      Minion.getModifiedAttack attrs `shouldReturn` 2
      Minion.getModifiedScheme attrs `shouldReturn` 2

  it "The Great Hunt gets one extra threat per tucked identity card" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      perform (L.tuckTopEncounterCard p)
      pass
      card <- revealNow p Cards.theGreatHuntSilk
      pass
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 3

  it "Atlas gains two maximum hit points per growth counter" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.atlasSilk
      resolve [PlaceCounters (MinionRef mid) 3]
      attrs <- minionAttrs mid
      Minion.getModifiedHitPoints attrs `shouldReturn` 24
      getCountersOn (MinionRef mid) `shouldReturn` 3

  it "Grow Invulnerable enters with additional threat equal to Atlas's growth" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.atlasSilk
      resolve [PlaceCounters (MinionRef mid) 2]
      card <- revealNow p Cards.growInvulnerableSilk
      pass
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 7

  it "Silk Sense Overload redirects player tucks and must leave after the third" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Cards.silkSenseOverloadSilk
      pass
      let oid = ObligationId $ unCardId $ ecCardId card
          tuckAndAnswer = do
            perform (L.tuckTopEncounterCard p)
            choose "Silk Sense Overload" (isAbilityOf (ObligationRef oid))
      tuckAndAnswer
      pass
      obligationsInPlay `shouldReturn` 1
      L.tuckedEncounterCards p `shouldReturn` []
      tuckAndAnswer
      choose "keep at two" (isLabel "$label.done")
      pass
      obligationsInPlay `shouldReturn` 1
      tuckAndAnswer
      pass
      obligationsInPlay `shouldReturn` 0

  it "Silk Sense Overload does not redirect Hunting the Spider-Bride's encounter tuck" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.silkSenseOverloadSilk
      pass
      card <- revealNow p Cards.huntingtheSpiderBrideSilk
      pass
      tucked <- L.tuckedEncounterCards p
      tucked `shouldSatisfy` elem card
      obligationsInPlay `shouldReturn` 1

  it "Growing Strong engages an existing Atlas and activates him against the revealer" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      mid <- addMinion p Cards.atlasSilk
      _ <- setEncounterDeck [Treacheries.advance,Treacheries.assault] []
      _ <- revealNow p Cards.growingStrongSilk
      choose "no defender" isNoDefender
      pass
      selectAny (MinionWithId mid <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True
      damageOf p `shouldReturn` 3

  it "Titanic Proportions deals group indirect damage equal to Atlas's growth" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.atlasSilk
      resolve [PlaceCounters (MinionRef mid) 3]
      _ <- revealNow p Cards.titanicProportionsSilk
      pass
      damageOf p `shouldReturn` 3

  it "Titanic Proportions surges when Atlas has fewer than three growth counters" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      _ <- setEncounterDeck [Treacheries.advance,Treacheries.advance,Treacheries.advance] []
      before <- mainSchemeThreatNow
      _ <- revealNow p Cards.titanicProportionsSilk
      pass
      damageOf p `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before
      length . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` 1

  it "Grow Invulnerable loses the game as soon as Atlas has ten growth counters" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.atlasSilk
      _ <- revealNow p Cards.growInvulnerableSilk
      pass
      resolve [PlaceCounters (MinionRef mid) 10]
      getsGame gameState `shouldReturn` Finished Lost

  it "Grow Invulnerable's loss condition ends when that side scheme leaves play" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      mid <- addMinion p Cards.atlasSilk
      card <- revealNow p Cards.growInvulnerableSilk
      pass
      resolve [RemoveFromPlay $ SideSchemeRef $ sideSchemeOf card]
      resolve [PlaceCounters (MinionRef mid) 10]
      getsGame gameState `shouldReturn` InProgress

  it "Silk Sense Overload may be discarded at exactly two tucked cards" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      card <- revealNow p Cards.silkSenseOverloadSilk
      pass
      let oid = ObligationId $ unCardId $ ecCardId card
      perform (L.tuckTopEncounterCard p)
      choose "first redirected tuck" (isAbilityOf (ObligationRef oid))
      pass
      perform (L.tuckTopEncounterCard p)
      choose "second redirected tuck" (isAbilityOf (ObligationRef oid))
      choose "discard Silk Sense Overload" (isTargetChoice (ObligationRef oid))
      pass
      obligationsInPlay `shouldReturn` 0
      discard <- getEncounterDiscard
      discard `shouldSatisfy` elem card

  it "Morlun's defeat discards tucked Hunting copies without their player-effect damage" do
    tg <- testGame ["silk"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      clearHand p
      hunted <- encounterCard Cards.huntingtheSpiderBrideSilk
      perform (L.tuckEncounterCard p hunted)
      pass
      mid <- addMinion p Cards.morlunSilk
      kick <- putInHand p "52003"
      payment <- traverse (putInHand p) ["01088","01089"]
      playWith kick payment
      choose "attack Morlun" (isTargetChoice mid)
      choose "do not spend Hunting" (isLabel "$label.attackWithoutDiscarding")
      pass
      L.tuckedEncounterCards p `shouldReturn` []
      damageOf p `shouldReturn` 0
