module Marvel.Rules.X23MechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Vector qualified as Vector
import Data.List (find, nub)
import Marvel.Card.Def (cdKeywords)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query (selectAny)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.Keyword (Keyword (Linked))
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Window qualified as W
import TestImport

trainingGame :: [DeckList] -> IO TestGame
trainingGame decks = do
  let setup = fromMaybe (error "no Rhino setup") $ recommendedSetup "01094"
  (gameApp, ran, logged) <- dealtGameLogged setup decks 1
  seats <- runGameApp gameApp (getsGame gameSeats)
  let tg = TestGame gameApp ran logged seats
  inGame tg toFirstTurn
  pure tg

withTraining :: DeckList -> DeckList
withTraining deck = deck {cards = deck.cards <> [("43021", 1)]}

specialists :: GameAppT [PlayerCard]
specialists = filter (any (\case Linked "43021" -> True; _ -> False) . cdKeywords . getCardDef) <$> getSetAsidePlayerCards

attachNanoSentinelTech :: MinionId -> GameAppT EncounterCard
attachNanoSentinelTech mid = do
  card <- encounterCard Attachments.mgNanoSentinelTech
  let attached = createAttachment card
  withGame_ $ entitiesL . attachmentsL . at (toId attached) ?~ attached
  resolve [AttachmentMessage (toId attached) $ AttachedToEnemy $ EnemyMinionId mid]
  pure card

spec :: Spec
spec = describe "X23 general mechanics" do
  it "sets aside the four linked specializations for a deck containing Specialized Training" do
    tg <- trainingGame [withTraining $ starter "spider-man"]
    inGame tg do
      cards <- specialists
      map toCardCode cards `shouldMatchList` ["43034", "43035", "43036", "43037"]
      map pcOwner cards `shouldBe` replicate 4 Nothing

  it "sets aside a separate linked set for each deck containing Specialized Training" do
    tg <- trainingGame [withTraining $ starter "spider-man", withTraining $ starter "captain-marvel"]
    inGame tg do
      cards <- specialists
      map toCardCode cards `shouldMatchList` concat (replicate 2 ["43034", "43035", "43036", "43037"])
      length (nub $ map pcCardId cards) `shouldBe` 8

  it "sets aside no linked cards when no deck contains their parent" do
    tg <- testGame ["spider-man"]
    inGame tg $ specialists `shouldReturn` []

  it "gives a linked upgrade to its new owner and removes it from set-aside" do
    tg <- trainingGame [withTraining $ starter "spider-man", starter "captain-marvel"]
    inGame tg do
      cards <- specialists
      let card = fromMaybe (error "no combat specialist") $ find ((== "43034") . toCardCode) cards
          owner = player2 tg
          uid = UpgradeId $ unCardId $ pcCardId card
      resolve [PutCardIntoPlay owner card NoPayment Nothing False]
      getsGame (fmap (Upgrade.upgradeOwner . toAttrs) . HashMap.lookup uid . gameUpgrades) `shouldReturn` Just owner
      map pcCardId <$> specialists `shouldReturn` map pcCardId (filter ((/= pcCardId card) . pcCardId) cards)

  it "rejects a linked card included directly in a deck" do
    (deckBuildingError ((starter "spider-man") {cards = [("43034", 1)]}) `shouldBe` Just "error.linkedCardInDeck") :: IO ()

  it "records attack completion independently of damage and clears it when the next turn begins" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      enemy <- EnemyVillainId <$> villainIdNow
      resolve [CheckWindows [W.Window W.After $ W.PlayerAttackResolved (IdentityRef you) you [enemy]]]
      getAttackTargetsThisTurn (IdentityRef you) `shouldReturn` [enemy]
      resolve [IdentityMessage you BeginTurn]
      getAttackTargetsThisTurn (IdentityRef you) `shouldReturn` []

  it "remembers a basic attack whose damage was prevented by tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      villain <- villainIdNow
      resolve [VillainMessage villain VillainBecomeTough]
      choose "basic attack" (isHeroAttack you)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 0
      getAttackTargetsThisTurn (IdentityRef you) `shouldReturn` [EnemyVillainId villain]

  it "attributes an event attack to its identity and an ally attack to that ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      enemy <- EnemyVillainId <$> villainIdNow
      event <- putInHand you "01006"
      allyCard <- putIntoPlay you "01002"
      let aid = allyIdOf allyCard
      resolve [CheckWindows [W.Window W.After $ W.PlayerAttackResolved (EventRef $ EventId $ unCardId $ pcCardId event) you [enemy]]]
      resolve [CheckWindows [W.Window W.After $ W.PlayerAttackResolved (AllyRef aid) you [enemy]]]
      getAttackTargetsThisTurn (IdentityRef you) `shouldReturn` [enemy]
      getAttackTargetsThisTurn (AllyRef aid) `shouldReturn` [enemy]

  it "matches only the physical card whose discard is being replaced" do
    tg <- testGame ["spider-man"]
    inGame tg do
      first <- encounterCard Minions.hydraMercenary
      second <- encounterCard Minions.hydraMercenary
      let matches card = W.windowMatches (W.CardWithIdWouldBeDiscardedFromPlay $ toCardId first)
            (W.Window W.Would $ W.WouldBeDiscardedFromPlay $ Right card) GameRef
      matches first `shouldReturn` True
      matches second `shouldReturn` False

  it "distinguishes identity attack responses from an ally's attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      enemy <- EnemyVillainId <$> villainIdNow
      ally <- putIntoPlay you "01002"
      let matches source = W.windowMatches (W.IdentityAttackResolved $ IdentityWithId you)
            (W.Window W.After $ W.PlayerAttackResolved source you [enemy]) GameRef
      matches (IdentityRef you) `shouldReturn` True
      matches (AllyRef $ allyIdOf ally) `shouldReturn` False

  it "matches any positive ally damage but not zero damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      ally <- putIntoPlay (player1 tg) "01002"
      let aid = allyIdOf ally
          matches damage = W.windowMatches (W.AllyTookDamageOf (AllyWithId aid) FromAnyDamageSource)
            (W.Window W.After $ W.AllyTookDamage aid GameRef damage) GameRef
      matches (toDamage 1 FromAbility) `shouldReturn` True
      matches (toDamage 1 FromConsequential) `shouldReturn` True
      matches (toDamage 0 FromAbility) `shouldReturn` False


  it "loads old queued defense-completion messages and preserves new basic-defense snapshots" do
    tg <- testGame ["spider-man"]
    inGame tg do
      enemy <- EnemyVillainId <$> villainIdNow
      let basic = AttackDefended enemy 0 True
          legacy = case toJSON basic of
            Object fields | Just (Array args) <- KeyMap.lookup "contents" fields ->
              Object $ KeyMap.insert "contents" (Array $ Vector.take 2 args) fields
            other -> other
      fromJSON legacy `shouldBe` Success (AttackDefended enemy 0 False)
      fromJSON (toJSON basic) `shouldBe` Success basic


  it "puts a Victory attachment into the victory display when its ordinary minion host is defeated" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you Minions.hydraMercenary
      tech <- attachNanoSentinelTech mid
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 99 $ FromPlayerAttack you]
      map toCardCode <$> getVictoryDisplay `shouldReturn` [toCardCode tech]
      discarded <- encounterDiscardCodes
      ("01101" `elem` discarded) `shouldBe` True
      (toCardCode tech `elem` discarded) `shouldBe` False

  it "puts both the Victory minion and its Victory attachment into the victory display" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      mid <- addMinion you Minions.badoonHeadhunter
      tech <- attachNanoSentinelTech mid
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 99 $ FromPlayerAttack you]
      victory <- getVictoryDisplay
      map toCardCode victory `shouldMatchList` ["16183", toCardCode tech]
      selectAny (MinionWithId mid) `shouldReturn` False

  it "discards a Victory attachment normally if its host leaves play without being defeated" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Minions.hydraMercenary
      tech <- attachNanoSentinelTech mid
      resolve [RemoveFromPlay $ MinionRef mid]
      map toCardCode <$> getVictoryDisplay `shouldReturn` []
      discarded <- encounterDiscardCodes
      (toCardCode tech `elem` discarded) `shouldBe` True
