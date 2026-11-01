module Marvel.Rules.GroundedTimingSpec (spec) where

import Marvel.ActiveCost.Types (ActiveCost(..))
import Marvel.Effect.Types
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Window qualified as W
import TestImport

-- A passive observer records actual before-change windows without inserting
-- another player ability or consuming a response in the window being tested.
newtype FormWindowProbe = FormWindowProbe (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage FormWindowProbe where
  runMessage msg effect@(FormWindowProbe attrs) = L.runQueueT $ case msg of
    CheckWindows windows -> do
      for_ [who | W.Window W.When (W.IdentityChangesForm who) <- windows, IdentityRef who == effectSource attrs] $ \who -> L.placeCounters who 1
      pure effect
    _ -> FormWindowProbe <$> runMessage msg attrs

installProbe :: IdentityId -> GameAppT ()
installProbe p = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (FormWindowProbe EffectAttrs
    { effectId = eid
    , effectCardCode = "47001a"
    , effectSource = IdentityRef p
    , effectMatcher = IdentityEntity $ IdentityWithId p
    , effectModifiers = []
    , effectEnds = Nothing
    })

beforeWindows :: IdentityId -> GameAppT Natural
beforeWindows p = playerIdentityCounters <$> playerAttrs p

spec :: Spec
spec = describe "Grounded form-change payment timing" do
  it "opens the normal form-change window only after the additional cost is paid" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jubilee47023
      pass
      clearHand p
      energy <- putInHand p "01088"
      installProbe p
      changeForm p
      beforeWindows p `shouldReturn` 0
      identityMatches AlterEgoIdentity p `shouldReturn` True
      payWith [energy]
      pass
      beforeWindows p `shouldReturn` 1
      identityMatches HeroIdentity p `shouldReturn` True

  it "does not open an effect's change window when its additional payment is aborted" do
    tg <- testGame ["jubilee"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Cards.jubilee47023
      pass
      clearHand p
      energy <- putInHand p "01088"
      installProbe p
      runNow [IdentityMessage p $ ChangedToForm A]
      beforeWindows p `shouldReturn` 0
      pending <- getsGame (headMay . gameActiveCost)
      case pending of
        Nothing -> fail "expected an additional form-change payment"
        Just cost -> do
          withGame_ $ questionL .~ mempty
          push $ AbortPayment $ activeCostId cost
          runGame
      pass
      beforeWindows p `shouldReturn` 0
      identityMatches AlterEgoIdentity p `shouldReturn` True
      runNow [IdentityMessage p $ ChangedToForm A]
      payWith [energy]
      pass
      beforeWindows p `shouldReturn` 1
      identityMatches HeroIdentity p `shouldReturn` True
