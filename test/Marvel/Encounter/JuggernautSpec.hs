module Marvel.Encounter.JuggernautSpec (spec) where
import Control.Monad (replicateM)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types hiding (AttachmentId)
import Marvel.EncounterSet qualified as Set
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

juggernautGame :: Difficulty -> [Text] -> IO TestGame
juggernautGame difficulty heroes = do
  let setup = ScenarioSetup "40118" [Set.BlackTomCassidy] difficulty
  (app,ran,logged) <- dealtGameLogged setup (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

attachmentWith :: CardCode -> GameAppT (Attrs Attachment)
attachmentWith code = getsGame (filter ((==code) . toCardCode . getCardDef) . map toAttrs . HashMap.elems . gameAttachments) >>= \case
  [a] -> pure a
  _ -> error "expected one attachment"

hasAttachment :: CardCode -> GameAppT Bool
hasAttachment code = getsGame (any ((==code) . toCardCode . getCardDef) . HashMap.elems . gameAttachments)

isAttachmentAbility :: AttachmentId -> Natural -> Choice -> Bool
isAttachmentAbility aid idx = \case
  UseAbility ab -> abilitySource ab == toSource aid && abilityIndex ab == idx
  _ -> False

momentum :: GameAppT Natural
momentum = villainIdNow >>= getCountersOn

flipHelmet :: GameAppT ()
flipHelmet = do
  a <- attachmentWith "40122a"
  resolve [cardSpecific (attachmentId a) "flipHelmet" ()]

settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = error "Juggernaut test did not settle"
  go n = openQuestion >>= \case
    Just (_,QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you,_) -> answerQuestion you 0 >> go (n-1)
    Nothing -> pure ()

finishForced :: GameAppT ()
finishForced = do
  pass
  openQuestion >>= \case
    Just (you,QuestionLabel "$label.resolveForced" _ _) -> answerQuestion you 0 >> finishForced
    _ -> pure ()

beforeDefense :: GameAppT ()
beforeDefense = asked >>= \case
  Just (_,choices) | any isDefend choices -> pure ()
  Just (you,choices) | Just n <- headMay [i | (i,Label label _) <- zip [0..] choices, label == "$label.noResponse"] -> answerQuestion you n >> beforeDefense
  _ -> pure ()

spec :: Spec
spec = describe "Juggernaut" do
  it "normal setup starts stage I with one momentum, tough, helmet and one encounter Hope" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      toCardCode . getCardDef <$> getVillain `shouldReturn` "40118"
      momentum `shouldReturn` 1
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True
      hasAttachment "40122a" `shouldReturn` True
      length <$> alliesWith "40130" `shouldReturn` 1
      mainSchemeThreat `shouldReturn` 1
  it "expert setup starts stage II and scales its hit points by starting player count" do
    tg <- juggernautGame Expert ["spider-man", "captain-marvel"]
    inGame tg do
      toCardCode . getCardDef <$> getVillain `shouldReturn` "40119"
      momentum `shouldReturn` 1
      vid <- villainIdNow
      getVillainHitPoints vid `shouldReturn` 42
      mainSchemeThreat `shouldReturn` 2
  it "each momentum counter adds one ATK" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      getVillainAttackValue vid `shouldReturn` 3
      resolve [PlaceCounters (toTarget vid) 2]
      getVillainAttackValue vid `shouldReturn` 5
  it "momentum carries to stage II, whose reveal adds another counter" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      resolve [PlaceCounters (toTarget vid) 2]
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 1 FromAbility)]
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 50 FromAbility)]
      pass
      toCardCode . getCardDef <$> getVillain `shouldReturn` "40119"
      momentum `shouldReturn` 4
  it "expert stage III reveals Head of Steam and carries existing momentum" do
    tg <- juggernautGame Expert ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      resolve [PlaceCounters (toTarget vid) 2]
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 1 FromAbility)]
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 50 FromAbility)]
      settle
      toCardCode . getCardDef <$> getVillain `shouldReturn` "40120"
      hasAttachment "40123" `shouldReturn` True
      momentum `shouldReturn` 4
  it "helmet grants stalwart and overkill" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      v <- getVillain
      keywords <- Villain.getModifiedKeywords (toAttrs v)
      keywords `shouldSatisfy` elem Stalwart
      keywords `shouldSatisfy` elem Overkill
  it "three same-type resources remove all momentum and expose Juggernaut" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      helmet <- attachmentWith "40122a"
      vid <- villainIdNow
      resolve [PlaceCounters (toTarget vid) 3]
      cards <- replicateM 3 (putInHand p "01003")
      choose "helmet physical payment" (isAttachmentAbility (attachmentId helmet) 1)
      payWith cards
      pass
      momentum `shouldReturn` 0
      hasAttachment "40122b" `shouldReturn` True
      keywords <- Villain.getModifiedKeywords . toAttrs =<< getVillain
      keywords `shouldSatisfy` notElem Stalwart
      keywords `shouldSatisfy` notElem Overkill
  it "helmet removal cannot mix three different non-wild resource types" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      helmet <- attachmentWith "40122a"
      _ <- putInHand p "01003"
      _ <- putInHand p "01005"
      _ <- putInHand p "01002"
      offers (isAttachmentAbility (attachmentId helmet) 1) `shouldReturn` False
      offers (isAttachmentAbility (attachmentId helmet) 2) `shouldReturn` False
      offers (isAttachmentAbility (attachmentId helmet) 3) `shouldReturn` False
  it "exposed Juggernaut takes one extra damage from a printed mental card, but not from other resources" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 1 FromAbility)]
      flipHelmet
      kick <- putInHand p "01005"
      pay <- putInHand p "01090"
      single <- putInHand p "01003"
      playWith kick [pay,single]
      pass
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 9
  it "exposed Juggernaut gains momentum and restores his helmet after scheming" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      flipHelmet
      _ <- stackEncounterDeck [Treacheries.imTough]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      hasAttachment "40122a" `shouldReturn` True
      momentum `shouldReturn` 2
  it "scheme completion resets threat, restores helmet, adds momentum and attacks even an alter ego" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      flipHelmet
      _ <- stackEncounterDeck [Treacheries.imTough]
      sid <- mainSchemeIdNow
      resolve [MainSchemeMessage sid (MainSchemePlaceThreat 6)]
      settle
      mainSchemeThreat `shouldReturn` 0
      hasAttachment "40122a" `shouldReturn` True
      momentum `shouldReturn` 2
      damageOf p `shouldReturn` 4
  it "Head of Steam adds momentum and grants retaliate equal to the current total" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Attachments.ne40123
      pass
      momentum `shouldReturn` 2
      keywords <- Villain.getModifiedKeywords . toAttrs =<< getVillain
      keywords `shouldSatisfy` elem (Retaliate 2)
  it "Head of Steam can be removed for zero resources after a zero-damage attack" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      steamCard <- revealNow p Attachments.ne40123
      pass
      vid <- villainIdNow
      resolve [RemoveCounters (toTarget vid) 2]
      _ <- stackEncounterDeck [Treacheries.imTough]
      runNow [VillainMessage vid (VillainAttacks p)]
      beforeDefense
      choose "defend" isDefend
      choose "discard Head of Steam" (isAttachmentAbility (AttachmentId $ unCardId $ ecCardId steamCard) 5)
      pass
      hasAttachment "40123" `shouldReturn` False
  it "Building Momentum may lose one threat after a hero defends a Juggernaut attack" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      scheme <- sideSchemeOf <$> revealNow p Schemes.ne40124
      pass
      _ <- stackEncounterDeck [Treacheries.imTough]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      beforeDefense
      choose "defend" isDefend
      choose "Building Momentum response" (isAbilityOf scheme)
      choose "remove its threat" (isTargetChoice scheme)
      pass
      sideSchemeThreat scheme `shouldReturn` 2
  it "Breakthrough can deal Juggernaut's current ATK as non-attack damage" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.ne40125
      choose "take ATK damage" (isLabel "$label.neTakeJuggernautDamage")
      pass
      damageOf p `shouldReturn` 3
  it "Breakthrough offers only the highest-cost upgrade or support across both types" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      mansion <- putIntoPlay p "01091"
      upgrade <- putIntoPlay p "01057"
      _ <- revealNow p Treacheries.ne40125
      choose "discard highest" (isLabel "$label.neDiscardHighest")
      pass
      discardOf p >>= (`shouldSatisfy` elem mansion)
      discardOf p >>= (`shouldSatisfy` notElem upgrade)
      damageOf p `shouldReturn` 0
  it "Flatten can add momentum instead of dealing its current ATK damage" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.ne40126
      choose "momentum" (isLabel "$label.nePlaceMomentum")
      pass
      momentum `shouldReturn` 2
      damageOf p `shouldReturn` 0
  it "Flatten can instead deal damage equal to current ATK without adding momentum" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.ne40126
      choose "damage" (isLabel "$label.neTakeJuggernautDamage")
      pass
      momentum `shouldReturn` 1
      damageOf p `shouldReturn` 3
  it "Flatten's boost gives Juggernaut tough" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 1 FromAbility)]
      _ <- stackEncounterDeck [Treacheries.ne40126]
      resolve [VillainMessage vid VillainSchemes]
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True
  it "Ground Pound assigns one shared indirect-damage total equal to current ATK" do
    tg <- juggernautGame Normal ["spider-man", "captain-marvel"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.ne40127
      settle
      heroDamage <- sum <$> traverse damageOf [p, player2 tg]
      allyDamage <- getsGame (sum . map (Ally.allyDamage . toAttrs) . HashMap.elems . gameAllies)
      (heroDamage + allyDamage) `shouldBe` 3
  it "Ground Pound's boost deals one indirect damage to the boosted player" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- stackEncounterDeck [Treacheries.ne40127]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      damageOf p `shouldReturn` 1
  it "Trample in alter ego deals two indirect damage instead of initiating an attack" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.ne40128
      settle
      heroDamage <- damageOf p
      allyDamage <- getsGame (sum . map (Ally.allyDamage . toAttrs) . HashMap.elems . gameAllies)
      (heroDamage + allyDamage) `shouldBe` 2
  it "Trample in hero form attacks the ally with fewest remaining hit points and allows defense" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      nick <- putIntoPlay p "01084"
      resolve [AllyMessage (allyIdOf nick) (AllySetDamage 2)]
      _ <- stackEncounterDeck [Treacheries.imTough]
      _ <- revealNow p Treacheries.ne40128
      beforeDefense
      choose "defend Nick" isDefend
      pass
      alliesWith "01084" >>= (`shouldSatisfy` ((==1) . length))
      damageOf p `shouldReturn` 0
  it "Cyttorak's Exemplar flips exposed Juggernaut and adds momentum without adding ATK threat" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      flipHelmet
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.ne40129
      pass
      hasAttachment "40122a" `shouldReturn` True
      momentum `shouldReturn` 2
      mainSchemeThreat `shouldReturn` before
  it "Cyttorak's Exemplar adds ATK threat while the helmet is on" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Treacheries.ne40129
      pass
      mainSchemeThreat `shouldReturn` 4
      momentum `shouldReturn` 1
  it "Cyttorak's Exemplar has incite one only in expert mode" do
    tg <- juggernautGame Expert ["spider-man"]
    inGame tg do
      let p = player1 tg
      flipHelmet
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.ne40129
      pass
      mainSchemeThreat `shouldReturn` (before + 1)
  it "Hope matches her controller's modified hero powers and takes no consequential damage" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- putIntoPlay p "01057"
      hope <- allyIn "40130"
      Ally.getModifiedAttack hope `shouldReturn` 3
      Ally.getModifiedThwart hope `shouldReturn` 1
      Ally.allyAttackConsequentialDamage hope `shouldBe` 0
      Ally.allyThwartConsequentialDamage hope `shouldBe` 0
  it "Hope does not consume an ally-limit slot and follows the first-player token" do
    tg <- juggernautGame Normal ["spider-man", "captain-marvel"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "01084"
      _ <- putIntoPlay p "01083"
      _ <- putIntoPlay p "01068"
      length <$> getsGame (HashMap.elems . gameAllies) `shouldReturn` 4
      resolve [PassFirstPlayer]
      Ally.allyController <$> allyIn "40130" `shouldReturn` player2 tg
  for_ ["discard", "return", "remove", "victory"] $ \departure ->
    it ("loses immediately when Hope leaves play by " <> departure) do
      tg <- juggernautGame Normal ["spider-man"]
      inGame tg do
        hope <- allyIn "40130"
        let ref = AllyRef $ Ally.allyId hope
            message = case departure of
              "discard" -> RemoveFromPlay ref
              "return" -> ReturnToHand ref
              "remove" -> RemoveFromGame ref
              _ -> AddToVictoryDisplay ref
        resolve [message]
        getsGame gameState `shouldReturn` Finished Lost
  it "Captive Hope exhausts Hope and prevents her from readying until defeated" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      scheme <- sideSchemeOf <$> revealNow p Schemes.ne40131
      pass
      Ally.allyExhausted <$> allyIn "40130" `shouldReturn` True
      hope <- allyIn "40130"
      resolve [AllyMessage (Ally.allyId hope) ReadiedAlly]
      Ally.allyExhausted <$> allyIn "40130" `shouldReturn` True
      resolve [SideSchemeMessage scheme (SideSchemeThwarted (toSource p) 3)]
      resolve [AllyMessage (Ally.allyId hope) ReadiedAlly]
      Ally.allyExhausted <$> allyIn "40130" `shouldReturn` False
  it "Black Tom reveals a Willow and cannot take damage while any Willow remains in play" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      tom <- minionOf <$> revealNow p Minions.ne40132
      pass
      willows <- selectList (MinionIs Minions.ne40133)
      length willows `shouldBe` 1
      resolve [MinionMessage tom (MinionDamaged (toSource p) (toDamage 5 FromAbility))]
      minionDamageOf tom `shouldReturn` Just 0
      for_ willows $ \mid -> resolve [RemoveFromPlay $ MinionRef mid]
      resolve [MinionMessage tom (MinionDamaged (toSource p) (toDamage 2 FromAbility))]
      minionDamageOf tom `shouldReturn` Just 2
  it "Black Tom is villainous and gets a boost card for his attack" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      tom <- minionOf <$> revealNow p Minions.ne40132
      pass
      asHero p
      _ <- stackEncounterDeck [Treacheries.explosion]
      runNow [MinionMessage tom (MinionAttacks p)]
      pass
      damageOf p `shouldReturn` 3
  it "Creeping Willow quickstrikes a hero and stuns the character it damages" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- revealNow p Minions.ne40133
      finishForced
      damageOf p `shouldReturn` 1
      isStunned p `shouldReturn` True
  it "Creeping Willow does not quickstrike an alter ego" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Minions.ne40133
      pass
      damageOf p `shouldReturn` 0
      isStunned p `shouldReturn` False
  it "Creeping Willow's boost stuns the boosted player" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- stackEncounterDeck [Minions.ne40133]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      isStunned p `shouldReturn` True
  it "Making Green enters with fixed two threat plus two per player from Hinder" do
    tg <- juggernautGame Normal ["spider-man", "captain-marvel"]
    inGame tg do
      scheme <- sideSchemeOf <$> revealNow (player1 tg) Schemes.ne40134
      pass
      sideSchemeThreat scheme `shouldReturn` 6
  it "Making Green grants Willow surge, dealing an additional facedown encounter card" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Schemes.ne40134
      pass
      _ <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Minions.ne40133
      pass
      dealtTo p >>= (`shouldSatisfy` any ((== "01101") . toCardCode . getCardDef))
  it "A Sound Thrashing makes engaged Willows attack even in alter ego" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion p Minions.ne40133
      _ <- revealNow p Treacheries.ne40135
      finishForced
      damageOf p `shouldReturn` 1
      isStunned p `shouldReturn` True
      length <$> selectList (MinionIs Minions.ne40133) `shouldReturn` 1
  it "A Sound Thrashing reveals a Willow if its player was not attacked" do
    tg <- juggernautGame Normal ["spider-man", "captain-marvel"]
    inGame tg do
      let p = player1 tg
      _ <- addMinion (player2 tg) Minions.ne40133
      _ <- revealNow p Treacheries.ne40135
      pass
      settle
      length <$> selectList (MinionIs Minions.ne40133) `shouldReturn` 2
      selectAny (MinionIs Minions.ne40133 <> MinionEngagedWith (IdentityWithId p)) `shouldReturn` True
  it "a stunned Willow did not attack, so A Sound Thrashing still reveals another Willow" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      willow <- addMinion p Minions.ne40133
      resolve [MinionMessage willow (MinionStunned $ toSource p)]
      _ <- revealNow p Treacheries.ne40135
      pass
      settle
      length <$> selectList (MinionIs Minions.ne40133) `shouldReturn` 2
      damageOf p `shouldReturn` 0
  it "Head of Steam charges for damage dealt even when Backflip prevents all damage taken" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      steamCard <- revealNow p Attachments.ne40123
      pass
      backflip <- putInHand p "01003"
      _ <- stackEncounterDeck [Treacheries.imTough]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      beforeDefense
      choose "no defender" isNoDefender
      playWith backflip []
      damageOf p `shouldReturn` 0
      offers (isAttachmentAbility (AttachmentId $ unCardId $ ecCardId steamCard) 5) `shouldReturn` False
      pass
      hasAttachment "40123" `shouldReturn` True
  it "normal stage II defeat wins rather than advancing to stage III" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 1 FromAbility)]
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 50 FromAbility)]
      pass
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 1 FromAbility)]
      resolve [VillainMessage vid (VillainDamaged (toSource p) $ toDamage 50 FromAbility)]
      settle
      getsGame gameState `shouldReturn` Finished Won

  it "Hope has zero base powers while her controller is in alter ego" do
    tg <- juggernautGame Normal ["spider-man"]
    inGame tg do
      hope <- allyIn "40130"
      Ally.getModifiedAttack hope `shouldReturn` 0
      Ally.getModifiedThwart hope `shouldReturn` 0
