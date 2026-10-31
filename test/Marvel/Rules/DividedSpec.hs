-- | The engine mechanics the Wasp pack builds on: damage and threat removal divided
-- among enemies and schemes, piercing basic attacks, defeats told by their source, a
-- basic power raised for one use, and an additional cost to make a basic attack.
module Marvel.Rules.DividedSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Control.Monad (replicateM_)
import Data.Text qualified as T
import Marvel.Cost.Types (Cost (ResourceCost))
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword (Keyword (Piercing))
import Marvel.Matchers.Types
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (..))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Villain.Types (villainIsTough)
import Marvel.Window qualified as W
import TestImport

-- | Modifiers given to an identity for the test, as a card in play would.
newtype TestModifiers = TestModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TestModifiers where
  runMessage msg (TestModifiers a) = TestModifiers <$> runMessage msg a

giveModifiers :: IdentityId -> [Modifier] -> GameAppT ()
giveModifiers ident ms = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestModifiers EffectAttrs
    { effectId = eid
    , effectCardCode = "01039"
    , effectSource = GameRef
    , effectMatcher = IdentityEntity (IdentityWithId ident)
    , effectModifiers = ms
    , effectEnds = Nothing
    })

-- | Spider-Man in hero form against Rhino.
spiderMan :: IO (TestGame, IdentityId)
spiderMan = do
  tg <- testGame ["spider-man"]
  let p = player1 tg
  inGame tg do
    changeForm p
    clearHand p
  pure (tg, p)

-- | Shown messages that mention the text, among the latest run.
ranWith :: TestGame -> Text -> IO [Text]
ranWith tg t = filter (\m -> "CheckWindows [" `T.isPrefixOf` m && t `T.isInfixOf` m) <$> readIORef tg.ran

spec :: Spec
spec = describe "divided damage and threat, and the Wasp pack's mechanics" do
  it "divided damage is assigned a point at a time, each enemy given some attacked" do
    (tg, p) <- spiderMan
    inGame tg do
      shocker <- addMinion p Minions.shocker
      v <- villainIdNow
      runNow [AssignDividedDamage p (IdentityRef p) (toDamage 3 (FromPlayerAttack p)) (AttackableBy (IdentityWithId p)) []]
      choose "the villain" (isTargetChoice v)
      choose "the villain" (isTargetChoice v)
      choose "Shocker" (isTargetChoice shocker)
      villainDamageNow `shouldReturn` 2
      minionDamageOf shocker `shouldReturn` Just 1
    attacked <- ranWith tg "Window {windowTiming = When, windowType = IdentityAttack"
    -- Simultaneous attacks may share one CheckWindows message.
    sum (map (T.count "Window {windowTiming = When, windowType = IdentityAttack") attacked) `shouldBe` 2

  it "a guard minion keeps the villain out of a divided attack" do
    (tg, p) <- spiderMan
    inGame tg do
      merc <- addMinion p Minions.hydraMercenary
      -- the one enemy that can be attacked takes it all, nothing asked
      resolve [AssignDividedDamage p (IdentityRef p) (toDamage 2 (FromPlayerAttack p)) (AttackableBy (IdentityWithId p)) []]
      villainDamageNow `shouldReturn` 0
      minionDamageOf merc `shouldReturn` Just 2

  it "a hero whose basic attack divides may divide it, or attack one enemy as ever" do
    (tg, p) <- spiderMan
    inGame tg do
      shocker <- addMinion p Minions.shocker
      v <- villainIdNow
      giveModifiers p [DividesBasicAttack]
      choose "attack" (isHeroAttack p)
      offers (isTargetChoice v) `shouldReturn` True
      choose "divide" (isLabel "$label.divideDamage")
      choose "the villain" (isTargetChoice v)
      choose "Shocker" (isTargetChoice shocker)
      villainDamageNow `shouldReturn` 1
      minionDamageOf shocker `shouldReturn` Just 1

  it "divided threat is removed a point at a time, none past a scheme's threat, each scheme thwarted" do
    (tg, p) <- spiderMan
    inGame tg do
      scheme <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
      placeMainThreat 3
      let side = SchemeSideSchemeId scheme
      before <- sideSchemeThreat scheme
      mainBefore <- mainSchemeThreatNow
      runNow [AssignDividedThreat p (IdentityRef p) True False (before + 1) ThwartableScheme []]
      replicateM_ (fromIntegral before) $ choose "the side scheme" (isTargetChoice side)
      -- the side scheme is out of threat: the rest goes to the main scheme, unasked
      mainSchemeThreatNow `shouldReturn` (mainBefore - 1)
      getsGame (HashMap.member scheme . gameSideSchemes) `shouldReturn` False
    -- its defeat is told with what removed its last threat
    defeated <- ranWith tg "DefeatedSideSchemeBy"
    defeated `shouldSatisfy` any ("(Just (IdentityRef" `T.isInfixOf`)

  it "a hero whose basic thwart divides is offered dividing it" do
    (tg, p) <- spiderMan
    inGame tg do
      _ <- revealNow p SideSchemes.bombScare
      placeMainThreat 3
      giveModifiers p [DividesBasicThwart]
      mainBefore <- mainSchemeThreatNow
      choose "thwart" (isHeroThwart p)
      choose "divide" (isLabel "$label.divideThreat")
      choose "the main scheme" . isTargetChoice =<< mainSchemeIdNow
      mainSchemeThreatNow `shouldReturn` (mainBefore - 1)

  it "a basic attack gains piercing: the villain's tough status card is discarded first" do
    (tg, p) <- spiderMan
    inGame tg do
      v <- villainIdNow
      resolve [GiveStatus (toTarget v) ToughStatus]
      giveModifiers p [KeywordModifier Piercing]
      choose "attack" (isHeroAttack p)
      chooseVillain
      villainDamageNow `shouldReturn` 2
      villainIsTough <$> getVillain `shouldReturn` False

  it "a defeat is matched by its source: the hero's own, or an event its player played" do
    (tg, p) <- spiderMan
    inGame tg do
      mid <- addMinion p Minions.shocker
      sid <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
      let wasp = SourceOneOf [SourceIsIdentity You, EventPlayedBy You]
          minionWindow s = W.Window W.After $ W.DefeatedMinion mid (toDamage 1 FromAbility) s 0
          schemeWindow s = W.Window W.After $ W.DefeatedSideSchemeBy sid p (Just s)
          matches m w = W.windowMatches m w GameRef
      matches (W.MinionDefeatedBy W.After AnyMinion wasp) (minionWindow (IdentityRef p)) `shouldReturn` True
      matches (W.SideSchemeDefeatedBy AnySideScheme wasp) (schemeWindow (IdentityRef p)) `shouldReturn` True
      -- an upgrade is not the hero
      upgrade <- upgradeIdOf <$> putIntoPlay p "01008"
      matches (W.MinionDefeatedBy W.After AnyMinion wasp) (minionWindow (UpgradeRef upgrade)) `shouldReturn` False
      -- an event, while it resolves, by the player who played it
      card <- putInHand p "01005"
      let event = createEvent p card
      withGame_ $ entitiesL . eventsL . at (toId event) ?~ event
      matches (W.SideSchemeDefeatedBy AnySideScheme wasp) (schemeWindow (EventRef $ toId event)) `shouldReturn` True
      matches (W.SideSchemeDefeatedBy AnySideScheme (SourceIsIdentity You)) (schemeWindow (EventRef $ toId event)) `shouldReturn` False

  it "a power raised as the hero changes form in its When window is the new form's, raised" do
    tg <- testGame ["ant-man"]
    let p = player1 tg
    inGame tg do
      changeFormTo p A
      v <- villainIdNow
      -- Tiny, ATK 1, changes to Giant, ATK 3, and gets +2 for this attack
      raised [IdentityMessage p (ChangedToForm C), IdentityMessage p (SideMessage (BasicAttack 0))] p 2
      chooseIf isNoResponse
      chooseVillain
      villainDamageNow `shouldReturn` 5
      -- Giant DEF 3, +2 for this defense
      raised [IdentityMessage p (SideMessage (DefenseApplied 0 (EnemyVillainId v)))] p 2
      (.playerIdentityDamageReduction) <$> playerAttrs p `shouldReturn` 5

  it "an additional cost to make a basic attack: one resource of any type, or no attack" do
    (tg, p) <- spiderMan
    inGame tg do
      giveModifiers p [BasicAttackAdditionalCost (ResourceCost Nothing)]
      offers (isHeroAttack p) `shouldReturn` False
      card <- putInHand p "01002"
      offers (isHeroAttack p) `shouldReturn` True
      choose "attack" (isHeroAttack p)
      choose "pay" (isPayWith card)
      chooseIf isFinishPayment
      chooseVillain
      villainDamageNow `shouldReturn` 2
      handOf p `shouldReturn` []
 where
  -- the messages run, the power waiting among them raised as an interrupt would
  raised msgs p n = do
    waiting <- openQuestion
    withGame_ $ questionL .~ mempty
    pushAll $ msgs <> [Ask ident q | Just (ident, q) <- [waiting]]
    addToBasicPower p n
    runGame
