module Marvel.Encounter.FearNoEvilAttackEffectsSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Query
import TestImport

spec :: Spec
spec = describe "Fear No Evil attack consequences" do
  it "Raise the Stakes shuffles back into the encounter deck after an attack that defeats nobody" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60065" Advanced]
      card <- revealNow you Cards.street60069
      pass
      let aid = AttachmentId $ unCardId $ ecCardId card
      _ <- setEncounterDeck (replicate 5 Cards.street60188) []
      resolve [VillainMessage vid $ VillainAttacks you]
      selectAny (AttachmentWithId aid) `shouldReturn` False
      getEncounterDeck >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId card]
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` [ecCardId card]

  it "Pursuit redirects the attack to its ally and Raise the Stakes removes both the defeated ally and itself" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60065" Advanced]
      ally <- putIntoPlay you "01084"
      pursuit <- revealNow you Cards.street60071
      choose "attach Pursuit to Nick Fury" (isTargetChoice $ allyIdOf ally)
      pass
      raise <- revealNow you Cards.street60069
      pass
      _ <- setEncounterDeck [Cards.street60116,Cards.street60188,Cards.street60188,Cards.street60188] []
      resolve [VillainMessage vid $ VillainAttacks you]
      selectAny (AllyWithId $ allyIdOf ally) `shouldReturn` False
      selectAny (AttachmentWithId $ AttachmentId $ unCardId $ ecCardId raise) `shouldReturn` False
      discardOf you >>= \cards -> liftIO $ map pcCardId cards `shouldNotContain` [pcCardId ally]
      getEncounterDiscard >>= \cards -> liftIO do
        map ecCardId cards `shouldContain` [ecCardId pursuit]
        map ecCardId cards `shouldNotContain` [ecCardId raise]
      getEncounterDeck >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` [ecCardId raise]
      damageOf you `shouldReturn` 2

  it "Energy Gatling Gun discards itself and makes Hammerhead attack the surviving target a second time" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60086" Advanced]
      card <- revealNow you Cards.street60089
      pass
      _ <- setEncounterDeck (replicate 5 Cards.street60188) []
      resolve [VillainMessage vid $ VillainAttacks you]
      damageOf you `shouldReturn` 4
      selectAny (AttachmentWithId $ AttachmentId $ unCardId $ ecCardId card) `shouldReturn` False
      getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldContain` [ecCardId card]
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1

  it "Head First gives a stunned hero an immediate Hammerhead attack with its two-point bonus" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      vid <- villainIdNow
      resolve [ReplaceVillain vid "60086" Advanced,GiveStatus (IdentityRef you) StunnedStatus]
      _ <- setEncounterDeck (replicate 4 Cards.street60188) []
      card <- encounterCard Cards.street60096
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 4
