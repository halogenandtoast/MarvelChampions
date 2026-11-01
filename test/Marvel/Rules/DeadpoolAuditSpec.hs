module Marvel.Rules.DeadpoolAuditSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (find)
import Marvel.Resource (resourcesFor)
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport

spec :: Spec
spec = describe "Deadpool audited interactions" do
  it "Frenemies damages Deadpool's hero and Cable's ally before thwarting both schemes" do
    tg <- testGame ["deadpool"]
    inGame tg do
      let you = player1 tg
      changeForm you
      pass
      clearHand you
      old <- getsGame $ HashMap.keys . gameSideSchemes
      resolve [RemoveFromPlay $ SideSchemeRef sid | sid <- old]
      _ <- putIntoPlay you "44002"
      side <- revealNow you Schemes.bombScare
      pass
      placeMainThreat 3
      frenemies <- putInHand you "44031"
      payment <- putInHand you "01091"
      playWith frenemies [payment]
      damageOf you `shouldReturn` 1
      allyIn "44002" >>= \ally -> allyDamage ally `shouldBe` 1
      choose "first scheme" (isTargetChoice $ sideSchemeOf side)
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` 0
      getsGame (HashMap.member (sideSchemeOf side) . gameSideSchemes) `shouldReturn` False

  for_ [("Self Confidence", "44025", Physical), ("Self Control", "44026", Mental), ("Self Preservation", "44027", Energy)] \(name, code, resource) ->
    it (name <> " follows the player who retrieves it through Plot Convenience") do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        let owner = player1 tg
            actor = player2 tg
        clearHand owner
        clearHand actor
        setDamage owner 5
        plot <- putIntoPlay owner "44050"
        card <- putInHand owner code
        choose "store through Plot Convenience" (isAbilityOfCard plot)
        choose "store an aspect card" (isLabel "$label.plotConvenienceStore")
        choose "store the resource" (isTargetChoice card)
        endTurn
        pass
        resolve [SupportMessage (supportIdOf plot) ReadiedSupport]
        choose "retrieve through another player's support" (isAbilityOfCard plot)
        choose "retrieve" (isLabel "$label.plotConvenienceRetrieve")
        choose "take the stored resource" (isTargetChoice card)
        hand <- handOf actor
        let received = fromMaybe (error "retrieved resource missing") $ find ((== pcCardId card) . pcCardId) hand
        pcOwner received `shouldBe` Just owner
        pcController received `shouldBe` Just actor
        resourcesFor received Nothing `shouldReturn` replicate 3 resource
        changeForm actor
        pass
        attack <- putInHand actor "44018"
        playWith attack [received]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 5

  it "discards a transferred event to its original owner after another player plays it" do
    tg <- testGame ["captain-marvel", "spider-man"]
    inGame tg do
      let owner = player1 tg
          actor = player2 tg
      clearHand owner
      clearHand actor
      plot <- putIntoPlay owner "44050"
      event <- putInHand owner "44018"
      choose "store the event" (isAbilityOfCard plot)
      choose "store" (isLabel "$label.plotConvenienceStore")
      choose "Cutupper" (isTargetChoice event)
      endTurn
      pass
      resolve [SupportMessage (supportIdOf plot) ReadiedSupport]
      choose "retrieve another player's event" (isAbilityOfCard plot)
      choose "retrieve" (isLabel "$label.plotConvenienceRetrieve")
      choose "Cutupper" (isTargetChoice event)
      actorHand <- handOf actor
      let received = fromMaybe (error "transferred event missing") $ find ((== pcCardId event) . pcCardId) actorHand
      changeForm actor
      pass
      payment <- traverse (putInHand actor) ["01088", "01090"]
      playWith received payment
      chooseVillain
      pass
      ownerDiscard <- discardOf owner
      actorDiscard <- discardOf actor
      (pcCardId event `elem` map pcCardId ownerDiscard) `shouldBe` True
      (pcCardId event `elem` map pcCardId actorDiscard) `shouldBe` False
