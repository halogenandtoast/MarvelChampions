module Marvel.Rules.ThwartInterruptSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (limitedWindowAbility)
import Marvel.Ability.Types (AbilityType (Action, Interrupt), HasAbilities (..))
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Criteria (Criteria (OwnsThis))
import Marvel.Matchers.Types
import Marvel.Message.Lifted qualified as L
import Marvel.Modifier (HasModifiersFor (..), Modifier (..))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Window qualified as W
import TestImport

-- A source-scoped bonus supplied by an interrupt. Keeping this independent of a
-- specific card verifies the timing contract used by Operative Skill.
data ThwartProbe = ThwartProbe (Attrs Upgrade) (Maybe Source)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsUpgrade ThwartProbe where
  toUpgradeAttrs (ThwartProbe attrs _) = attrs

instance HasModifiersFor ThwartProbe where
  getModifiersFor _ target (ThwartProbe _ source) =
    pure [ThreatRemovedModifier 1 | source == Just (toSource target)]

instance HasAbilities ThwartProbe where
  getAbilities (ThwartProbe attrs _) =
    [ limitedWindowAbility attrs 1
        (W.WindowOneOf [W.HeroThwarted W.When owner, W.IdentityThwarts W.When owner])
        Interrupt OwnsThis NoCost (runAbility attrs 1)
    ]
   where
    owner = IdentityWithId $ upgradeController attrs

instance RunMessage ThwartProbe where
  runMessage msg probe@(ThwartProbe attrs source) = L.runQueueT $ case msg of
    RanAbility you target 1 windows _ | target == toRef attrs -> do
      if W.MadeBasicThwart you `elem` windows
        then L.addToBasicPower you 1 >> pure probe
        else pure $ ThwartProbe attrs (headMay [from | W.IdentityThwart who from <- windows, who == you])
    _ -> (\a -> ThwartProbe a source) <$> runMessage msg attrs

installProbe :: IdentityId -> GameAppT PlayerCard
installProbe p = do
  card <- putIntoPlay p "01008"
  attrs <- getsGame (toAttrs . fromMaybe (error "missing thwart probe") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (ThwartProbe attrs Nothing)
  pure card

spec :: Spec
spec = describe "Gambit per-thwart interrupt timing" do
  it "recalculates the first event instance after its interrupt" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      probe <- installProbe p
      placeMainThreat 6
      before <- mainSchemeThreatNow
      event <- putInHand p "01060"
      energy <- putInHand p "01088"
      playWith event [energy]
      chooseMainScheme
      offers (isAbilityOfCard probe) `shouldReturn` True
      choose "increase this thwart" (isAbilityOfCard probe)
      pass
      mainSchemeThreatNow `shouldReturn` (before - 4)

  it "offers once for a multi-instance event and increases both instances" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      probe <- installProbe p
      side <- sideSchemeOf <$> revealNow p SideSchemes.bombScare
      resolve [SideSchemeMessage side (SideSchemePlaceThreat 5)]
      sideBefore <- sideSchemeThreat side
      placeMainThreat 6
      mainBefore <- mainSchemeThreatNow
      event <- putInHand p "15013"
      mental <- putInHand p "01089"
      playWith event [mental]
      chooseMainScheme
      choose "increase the whole thwart" (isAbilityOfCard probe)
      pass
      mainSchemeThreatNow `shouldReturn` (mainBefore - 3)
      offers (isAbilityOfCard probe) `shouldReturn` False
      choose "the different scheme" (isTargetChoice $ SchemeSideSchemeId side)
      sideSchemeThreat side `shouldReturn` (sideBefore - 3)

  it "adds a divided thwart's bonus once before distributing its total" do
    tg <- testGame ["phoenix"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      probe <- installProbe p
      placeMainThreat 6
      before <- mainSchemeThreatNow
      event <- putInHand p "34018"
      resource <- putInHand p "34011"
      playWith event [resource]
      offers (isAbilityOfCard probe) `shouldReturn` True
      choose "increase the distribution pool" (isAbilityOfCard probe)
      pass
      mainSchemeThreatNow `shouldReturn` (before - 4)
      offers (isAbilityOfCard probe) `shouldReturn` False

  it "opens for a labeled thwart while in alter-ego form" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      probe <- installProbe p
      placeMainThreat 6
      before <- mainSchemeThreatNow
      original <- putInHand p "01060"
      let event = original {pcCardDef = (getCardDef original) {cdCost = Just 0, cdAbilityType = Just Action}}
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) ->
        PlayerIdentity attrs {playerIdentityHand = Hand [event]}
      playWith event []
      chooseMainScheme
      offers (isAbilityOfCard probe) `shouldReturn` True
      choose "increase the alter-ego thwart" (isAbilityOfCard probe)
      pass
      mainSchemeThreatNow `shouldReturn` (before - 4)

  it "does not add another window to a basic thwart" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      probe <- installProbe p
      placeMainThreat 6
      before <- mainSchemeThreatNow
      choose "basic thwart" (isHeroThwart p)
      choose "increase the basic power" (isAbilityOfCard probe)
      pass
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` (before - 2)
      offers (isAbilityOfCard probe) `shouldReturn` False

  it "does not treat ordinary threat removal or an ally's thwart as your thwart" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      probe <- installProbe p
      ally <- putIntoPlay p "01002"
      placeMainThreat 6
      before <- mainSchemeThreatNow
      runNow [Ask p $ ChooseOne [RemoveThreat (UpgradeRef $ upgradeIdOf probe) 2 AnyScheme]]
      choose "remove threat without thwarting" (\case RemoveThreat {} -> True; _ -> False)
      chooseMainScheme
      offers (isAbilityOfCard probe) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` (before - 2)
      main <- mainSchemeIdNow
      runNow [Ask p $ ChooseOne [BasicThwartScheme (MainSchemeRef main) (AllyRef $ allyIdOf ally) 1]]
      choose "the ally thwarts" (\case BasicThwartScheme {} -> True; _ -> False)
      offers (isAbilityOfCard probe) `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` (before - 3)
