module Marvel.Rules.ValkyrieSearchCostSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Cost.Types (Cost (SearchEncounterMinionCost))
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword (Keyword (Retaliate))
import Marvel.Matchers.Types (EntityMatcher (IdentityEntity), IdentityMatcher (IdentityWithId))
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Modifier (Modifier (KeywordModifier))
import TestImport

newtype RetaliationFixture = RetaliationFixture (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage RetaliationFixture where
  runMessage msg (RetaliationFixture a) = RetaliationFixture <$> runMessage msg a

retaliateFour :: IdentityId -> GameAppT ()
retaliateFour p = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (RetaliationFixture EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = IdentityEntity (IdentityWithId p), effectModifiers = [KeywordModifier $ Retaliate 4]
    , effectEnds = Nothing })

searchingAssessment :: IdentityId -> GameAppT PlayerCard
searchingAssessment p = do
  original <- putInHand p "12033"
  let card = original {pcCardDef = (getCardDef original) {cdAbilityCost = SearchEncounterMinionCost}}
  withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
    PlayerIdentity a {playerIdentityHand = Hand (card : filter ((/= pcCardId card) . pcCardId) (unHand a.playerIdentityHand))}
  pure card

spec :: Spec
spec = describe "Valkyrie minion search costs" do
  it "does not resolve the event when uniqueness prevents the searched minion from entering" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      existing <- addMinion p Minions.shocker
      changeForm p
      clearHand p
      before <- handSizeOf p
      card <- searchingAssessment p
      (deck, _) <- setEncounterDeck [Minions.shocker, Schemes.bombScare] []
      playWith card []
      choose "attempt another unique Shocker" (isTargetChoice $ head deck)
      pass
      handSizeOf p `shouldReturn` before
      getsGame (HashMap.keys . gameMinions) `shouldReturn` [existing]
      handOf p >>= (`shouldSatisfy` elem card)
      encounterDiscard >>= (`shouldSatisfy` elem (head deck))

  it "pays the cost when the minion enters but later dies to retaliate during its quickstrike" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      retaliateFour p
      before <- handSizeOf p
      card <- searchingAssessment p
      (deck, _) <- setEncounterDeck [Minions.vulture, Schemes.bombScare] []
      playWith card []
      choose "put Vulture into play" (isTargetChoice $ head deck)
      handSizeOf p `shouldReturn` before
      choose "do not defend quickstrike" isNoDefender
      pass
      let mid = MinionId $ unCardId $ ecCardId $ head deck
      damageOf p `shouldReturn` 3
      getsGame (HashMap.member mid . gameMinions) `shouldReturn` False
      handSizeOf p `shouldReturn` before + 1
      discardOf p >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))
