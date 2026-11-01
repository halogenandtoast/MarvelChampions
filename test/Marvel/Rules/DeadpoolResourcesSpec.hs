module Marvel.Rules.DeadpoolResourcesSpec (spec) where

import Marvel.Area (AreaId (..), newArea)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Resource (resourcesFor)
import TestImport

-- Synthetic resource definitions isolate the generation rule from pack scaffolding.
resourceInHand :: IdentityId -> Resource -> [(ResourceRestriction, Resource)] -> GameAppT PlayerCard
resourceInHand you printed extra = do
  original <- putInHand you "01088"
  let card = original {pcCardDef = (getCardDef original) {cdResources = (PrintedResource, printed) : extra}}
  withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity attrs) ->
    PlayerIdentity attrs {playerIdentityHand = Hand (card : filter (/= original) (unHand attrs.playerIdentityHand))}
  pure card

selfResource :: IdentityId -> Resource -> GameAppT PlayerCard
selfResource you resource = resourceInHand you resource
  [(ResourceIfIdentityDamageLessThan 5, resource), (ResourceIfIdentityDamageLessThan 1, resource)]

spec :: Spec
spec = describe "Deadpool conditional resources" do
  it "generates three, two or one resources at the sustained-damage boundaries without adding printed icons" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      for_ [Physical, Mental, Energy] \resource -> do
        card <- selfResource you resource
        for_ [(0, 3), (1, 2), (4, 2), (5, 1)] \(damage, count) -> do
          setDamage you damage
          resourcesFor card Nothing `shouldReturn` replicate count resource
          printedResourcesFor card `shouldReturn` [resource]

  it "uses its owner's damage when another player is resolving the payment" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      card <- selfResource (player1 tg) Mental
      setDamage (player1 tg) 4
      setDamage (player2 tg) 0
      asYou (player2 tg) (resourcesFor card Nothing) `shouldReturn` [Mental, Mental]

  it "multiplies the replaced printed resource rather than generating its original resource type" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- selfResource you Physical
      _ <- revealNow you Attachments.haywire
      resourcesFor card Nothing `shouldReturn` [Energy, Energy, Energy]
      printedResourcesFor card `shouldReturn` [Energy]
      setDamage you 1
      resourcesFor card Nothing `shouldReturn` [Energy, Energy]

  it "lets an undamaged identity pay a three-resource attack using one conditional resource card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      card <- selfResource you Physical
      attack <- putInHand you "01054"
      playWith attack [card]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 5

  it "adds one wild per main-scheme acceleration token and caps the additional resources at three" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- resourceInHand you Wild [(ForEachMainSchemeAccelerationToken 3, Wild)]
      resourcesFor card Nothing `shouldReturn` [Wild]
      scheme <- mainSchemeIdNow
      resolve [PlaceAccelerationToken scheme]
      resourcesFor card Nothing `shouldReturn` [Wild, Wild]
      resolve $ replicate 3 (PlaceAccelerationToken scheme)
      resourcesFor card Nothing `shouldReturn` replicate 4 Wild
      printedResourcesFor card `shouldReturn` [Wild]

  it "counts tokens in its owner's area rather than the active player's area" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let owner = player1 tg
          other = player2 tg
          separate = (newArea (AreaId 99) [other]) {areaAccelerationTokens = 3}
      card <- resourceInHand owner Wild [(ForEachMainSchemeAccelerationToken 3, Wild)]
      withGame_ $ areasL %~ \areas ->
        [a {areaPlayers = filter (/= other) (areaPlayers a), areaAccelerationTokens = 1} | a <- areas] <> [separate]
      asYou other (resourcesFor card Nothing) `shouldReturn` [Wild, Wild]

  it "does not generate resources from self-payment or an uncontrolled card outside every hand" do
    tg <- testGame ["spider-man"]
    inGame tg do
      card <- selfResource (player1 tg) Energy
      resourcesFor card (Just card) `shouldReturn` []
      clearHand (player1 tg)
      resourcesFor (card {pcOwner = Nothing, pcController = Nothing}) Nothing `shouldReturn` [Energy]
