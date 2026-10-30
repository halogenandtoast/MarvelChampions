module Marvel.Rules.AllyLeavesPlaySpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (limitedWindowAbility)
import Marvel.Ability.Types (AbilityType (Interrupt, Response), HasAbilities (..))
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Criteria (Criteria (NoCriteria))
import Marvel.Effect.Types qualified as Effect
import Marvel.Matchers.Types
import Marvel.Modifier (HasModifiersFor (..), Modifier (RemovedFromGameInsteadOfLeavingPlay))
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Window qualified as W
import TestImport

-- Responses listen to a captured controller and card definition after it has gone.
data DepartureProbe = DepartureProbe (Attrs Upgrade) [Ability]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasModifiersFor)
instance IsUpgrade DepartureProbe where
  toUpgradeAttrs (DepartureProbe a _) = a
instance HasAbilities DepartureProbe where
  getAbilities (DepartureProbe _ abilities) = abilities
instance RunMessage DepartureProbe where
  runMessage msg (DepartureProbe attrs abilities) =
    (\a -> DepartureProbe a abilities) <$> runMessage msg attrs

probe :: IdentityId -> W.WindowTiming -> CardCode -> GameAppT PlayerCard
probe p timing code = do
  card <- putIntoPlay p code
  attrs <- getsGame (toAttrs . fromMaybe (error "missing probe") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  let ab = limitedWindowAbility attrs 1 (W.AllyLeavesPlay timing (IdentityWithId p) AnyCard)
        (if timing == W.When then Interrupt else Response) NoCriteria NoCost
        (Run [IdentityMessage p ReadiedIdentity])
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (DepartureProbe attrs [ab])
  pure card

newtype LeaveReplacement = LeaveReplacement (Attrs Effect.Effect)
  deriving anyclass (Effect.IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage LeaveReplacement where
  runMessage msg (LeaveReplacement a) = LeaveReplacement <$> runMessage msg a

spec :: Spec
spec = describe "all-cause ally leaves-play windows" do
  for_ [("discard", RemoveFromPlay), ("return to hand", ReturnToHand), ("remove from game", RemoveFromGame), ("victory display", AddToVictoryDisplay)] \(cause, leave) ->
    it ("opens an interrupt before and a snapshot response after " <> cause) do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        before <- probe p W.When "01008"
        after <- probe p W.After "01093"
        ally <- putIntoPlay p "01068"
        let aid = allyIdOf ally
        runNow [leave (AllyRef aid)]
        offers (isAbilityOfCard before) `shouldReturn` True
        offers (isAbilityOfCard after) `shouldReturn` False
        getsGame (HashMap.member aid . gameAllies) `shouldReturn` True
        choose "answer before departure" (isAbilityOfCard before)
        offers (isAbilityOfCard after) `shouldReturn` True
        getsGame (HashMap.member aid . gameAllies) `shouldReturn` False
        windows <- getCurrentWindows
        windows `shouldSatisfy` any (\case
          W.Window W.After (W.AllyLeavingPlay controller departed def) -> controller == p && departed == aid && toCardCode def == "01068"
          _ -> False)
        choose "answer after departure" (isAbilityOfCard after)
        pass
        when (cause == "victory display") $ do
          cards <- getVictoryDisplay
          cards `shouldSatisfy` any ((== pcCardId ally) . ecCardId)

  it "does not reopen departure when removal replaces a discard" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      before <- probe p W.When "01008"
      after <- probe p W.After "01093"
      ally <- putIntoPlay p "01068"
      eid <- getRandom
      withGame_ $ entitiesL . effectsL . at eid ?~ Effect.Effect (LeaveReplacement Effect.EffectAttrs
        { Effect.effectId = eid, Effect.effectCardCode = "01039", Effect.effectSource = GameRef
        , Effect.effectMatcher = AllyEntity (AllyWithId $ allyIdOf ally)
        , Effect.effectModifiers = [RemovedFromGameInsteadOfLeavingPlay], Effect.effectEnds = Nothing })
      runNow [RemoveFromPlay $ AllyRef $ allyIdOf ally]
      choose "interrupt the one departure" (isAbilityOfCard before)
      offers (isAbilityOfCard before) `shouldReturn` False
      choose "respond to the one departure" (isAbilityOfCard after)
      pass
      discardOf p >>= (`shouldSatisfy` notElem ally)

  it "does not offer departure for a card that has already gone" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01068"
      resolve [RemoveFromPlay $ AllyRef $ allyIdOf ally]
      before <- probe p W.When "01008"
      runNow [RemoveFromPlay $ AllyRef $ allyIdOf ally]
      offers (isAbilityOfCard before) `shouldReturn` False
