module Marvel.Rules.IronheartMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Marvel.ActiveCost.Types
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Cost (canAffordCost)
import Marvel.Cost.Types
import Marvel.Keyword (Keyword (Guard, Retaliate))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Card.PlayerCard (printedResourcesFor)
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Game.Public (publicGameFor)
import Marvel.Identity (ironheartVersion)
import Marvel.Matchers.Types
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.Modifier
import Marvel.Resource (resourcesFor)
import TestImport

newtype IronheartModifiers = IronheartModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage IronheartModifiers where
  runMessage msg (IronheartModifiers attrs) = IronheartModifiers <$> runMessage msg attrs

modifyEntity :: EntityMatcher -> [Modifier] -> GameAppT ()
modifyEntity matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (IronheartModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = modifiers, effectEnds = Nothing })

fieldOf :: Text -> Value -> Maybe Value
fieldOf key (Object fields) = KeyMap.lookup (Key.fromText key) fields
fieldOf _ _ = Nothing

spec :: Spec
spec = describe "Ironheart engine mechanics" do
  it "swaps both identity faces while preserving counters, damage, statuses, and exhaustion" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
        { playerIdentityCounters = 8, playerIdentityDamage = 3, playerIdentityExhausted = True
        , playerIdentityStunned = True, playerIdentityConfused = True, playerIdentityTough = True }
      resolve [IdentityMessage p $ SwapIdentityVersion "01029a" "01029b"]
      attrs <- toAttrs <$> getPlayer p
      playerIdentitySide attrs `shouldBe` B
      playerIdentityCounters attrs `shouldBe` 8
      playerIdentityDamage attrs `shouldBe` 3
      playerIdentityExhausted attrs `shouldBe` True
      playerIdentityStunned attrs `shouldBe` True
      playerIdentityConfused attrs `shouldBe` True
      playerIdentityTough attrs `shouldBe` True
      toCardCode <$> getPlayer p `shouldReturn` "01029b"
      changeForm p
      toCardCode <$> getPlayer p `shouldReturn` "01029a"

  it "reads Ironheart's version from either face after swapping" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      for_ [("29001a", "29001b", 1), ("29002a", "29002b", 2), ("29003a", "29003b", 3)] \(hero, ae, version) -> do
        resolve [IdentityMessage p $ SwapIdentityVersion hero ae]
        ironheartVersion p `shouldReturn` version
      changeForm p
      ironheartVersion p `shouldReturn` 3

  it "does not expose the live encounter deck top to another viewer" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      modifyEntity (IdentityEntity $ IdentityWithId p) [CanLookAtTopOfEncounterDeck]
      g <- getGame
      mine <- liftIO $ publicGameFor (== p) g
      theirs <- liftIO $ publicGameFor (== other) g
      fieldOf "encounterDeckTop" mine `shouldSatisfy` (\x -> x /= Just Null && isJust x)
      fieldOf "encounterDeckTop" theirs `shouldBe` Just Null

  it "preserves printed-card resource overrides when a card leaves hand to pay" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      event <- putInHand p "01005"
      payment <- putInHand p "01089"
      extra <- putInHand p "01083"
      _ <- revealNow p Attachments.haywire
      printedResourcesFor payment `shouldReturn` [Energy, Energy]
      resourcesFor payment (Just event) `shouldReturn` [Energy, Energy]
      playWith event [payment, extra]
      chooseVillain
      villainDamageNow `shouldReturn` 8

  it "blanking a minion removes its printed keywords while preserving status cards" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      minion <- addMinion p Minions.armoredGuard
      modifyEntity (MinionEntity $ MinionWithId minion) [Blank]
      attrs <- getsGame $ toAttrs . fromMaybe (error "missing minion") . HashMap.lookup minion . gameMinions
      Minion.getModifiedKeywords attrs >>= (`shouldSatisfy` notElem Guard)
      minionTough attrs `shouldBe` True

  it "blanking a side scheme removes its printed hazard icon" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- revealNow p SideSchemes.breakinAndTakin
      getHazardCount `shouldReturn` 1
      modifyEntity (SideSchemeEntity $ SideSchemeWithId $ sideSchemeOf card) [Blank]
      getHazardCount `shouldReturn` 0

  it "a character that cannot take damage cannot pay a damage cost" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      canAffordCost p GameRef (DamageCost 1) `shouldReturn` True
      modifyEntity (IdentityEntity $ IdentityWithId p) [CannotTakeDamage]
      canAffordCost p GameRef (DamageCost 1) `shouldReturn` False

  it "does not attack or trigger retaliate on enemies allocated zero damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      card <- putIntoPlay p "01084"
      first <- addMinion p Minions.hydraBomber
      second <- addMinion p Minions.hydraBomber
      zero <- addMinion p Minions.hydraBomber
      modifyEntity (AllyEntity $ AllyWithId $ allyIdOf card) [DividesBasicAttackEvenly, AttackModifier 1]
      modifyEntity (MinionEntity $ MinionWithId zero) [KeywordModifier $ Retaliate 2]
      ally <- allyIn "01084"
      choose "divide the attack" (isAllyAttack ally)
      villain <- villainIdNow
      choose "one damage to the villain" (isTargetChoice villain)
      choose "one damage to the first minion" (isTargetChoice first)
      choose "one damage to the second minion" (isTargetChoice second)
      villainDamageNow `shouldReturn` 1
      minionDamageOf zero `shouldReturn` Just 0
      map allyDamage <$> alliesWith "01084" `shouldReturn` [1]

  it "a discard-controlled-card cost ignores matching cards in hand and under another player's control" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
        cost = DiscardControlledCardCost (CardWithResource Energy)
    inGame tg do
      _ <- putInHand p "01093"
      _ <- putIntoPlay other "01093"
      canAffordCost p GameRef cost `shouldReturn` False
      card <- putIntoPlay p "01093"
      canAffordCost p GameRef cost `shouldReturn` True
      targets <- discardableControlledCardsMatching p (CardWithResource Energy)
      targets `shouldBe` [UpgradeRef $ upgradeIdOf card]

  it "pays a discard-controlled-card cost through the normal leave-play pipeline" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
        cost = DiscardControlledCardCost (CardWithResource Energy)
    inGame tg do
      card <- putIntoPlay p "01093"
      acid <- getRandom
      withGame_ $ activeCostL .~ [ActiveCost acid p ForTreachery cost NoPayment Nothing [] Nothing]
      runNow [PayCost acid cost]
      discardOf p >>= (`shouldSatisfy` elem card)
      map (toCardCode . upgradeCardDef) <$> upgradesWith "01093" `shouldReturn` []
