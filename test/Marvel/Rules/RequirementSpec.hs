module Marvel.Rules.RequirementSpec (spec) where

import Data.HashSet qualified as HashSet
import Marvel.Cost.Types
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Identity (getCardCost, playableTargetsWith)
import Marvel.Keyword (Keyword (Requirement, Victory))
import Marvel.Matchers.Types
import Marvel.Modifier
import Test.Hspec qualified as Hspec
import TestImport

newtype RequirementModifiers = RequirementModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RequirementModifiers where
  runMessage msg (RequirementModifiers attrs) = RequirementModifiers <$> runMessage msg attrs

modifiersFor :: IdentityId -> [Modifier] -> GameAppT ()
modifiersFor who modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (RequirementModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = IdentityEntity (IdentityWithId who)
    , effectModifiers = modifiers, effectEnds = Nothing })

requiredEvent :: IdentityId -> [Resource] -> GameAppT PlayerCard
requiredEvent who resources = do
  original <- putInHand who "01005"
  let card = original {pcCardDef = (getCardDef original) {cdKeywords = HashSet.singleton (Requirement resources)}}
  withGame_ $ entitiesL . playersL . ix who %~ \(PlayerIdentity attrs) ->
    PlayerIdentity attrs {playerIdentityHand = Hand (card : filter (/= original) (unHand attrs.playerIdentityHand))}
  pure card

spec :: Spec
spec = describe "Requirement resources" do
  it "constrains the printed resource cost without charging additional resources" do
    requirementResourceCost [Energy, Mental, Physical] 3
      `Hspec.shouldBe` [Just Energy, Just Mental, Just Physical]
    requirementResourceCost [Mental] 3 `Hspec.shouldBe` [Just Mental, Nothing, Nothing]

  it "retains required resources when reductions lower the cost below their number" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      card <- requiredEvent p [Mental, Energy]
      modifiersFor p [ResourceCostReduction 10]
      ident <- getPlayer p
      costResources <$> getCardCost ident card Nothing `shouldReturn` [Just Mental, Just Energy]

  it "does not offer a reduced card without its mandatory resource" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      card <- requiredEvent p [Physical]
      _ <- putInHand p "01089"
      modifiersFor p [ResourceCostReduction 10]
      ident <- getPlayer p
      playableTargetsWith False ident card `shouldReturn` []
      _ <- putInHand p "01090"
      playableTargetsWith False ident card `shouldReturn` [Nothing]

  it "cannot play a Requirement card while ignoring its resource cost" do
    tg <- testGame ["iron-man"]
    let p = player1 tg
    inGame tg do
      card <- requiredEvent p [Mental]
      ident <- getPlayer p
      playableTargetsWith True ident card `shouldReturn` []
      modifiersFor p [IgnoresResourceCost]
      playableTargetsWith False ident card `shouldReturn` []

  it "preserves negative victory points through serialization and scoring" do
    let card = (lookupPlayerCard "01005") {cdKeywords = HashSet.singleton (Victory (-1))}
    (fromJSON (toJSON card) :: Result CardDef) `Hspec.shouldBe` Success card
    victoryValue card `Hspec.shouldBe` (-1)
