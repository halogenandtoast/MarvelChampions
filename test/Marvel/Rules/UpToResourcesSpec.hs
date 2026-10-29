module Marvel.Rules.UpToResourcesSpec (spec) where

import Marvel.ActiveCost (allocationLabel)
import Marvel.Ability
import Marvel.Ally.Types qualified as Ally
import Marvel.Ally.Runner ()
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Modifier
import Marvel.Payment.Types
import Marvel.Question (runAbility)
import TestImport

-- A test ally exposes the resource payment independently of Machine Man's interrupt.
newtype BoundedAlly = BoundedAlly (Attrs Ally)
  deriving anyclass (Ally.IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities BoundedAlly where
  getAbilities a = [ability a 1 Action OwnsThis (UpToResourcesCost 3) (runAbility a 1)]

instance HasModifiersFor BoundedAlly where
  getModifiersFor _ target (BoundedAlly a)
    | target == toRef a = pure [AttackModifier (Ally.allyCounters a)]
  getModifiersFor _ _ _ = pure []

instance RunMessage BoundedAlly where
  runMessage msg (BoundedAlly a) = case msg of
    RanAbility _ target 1 _ payment | target == toRef a ->
      pure $ BoundedAlly (a {Ally.allyCounters = paymentX payment})
    _ -> BoundedAlly <$> runMessage msg a

boundedAlly :: IdentityId -> GameAppT PlayerCard
boundedAlly p = do
  card <- putIntoPlay p "01068"
  withGame_ $ entitiesL . alliesL . ix (allyIdOf card) %~ \(Ally.Ally a) ->
    Ally.Ally (BoundedAlly ((Ally.toAllyAttrs a) {Ally.allyCounters = 9}))
  pure card

activate :: PlayerCard -> GameAppT ()
activate ally = choose "use bounded resource ability" \case
  UseAbility a -> abilitySource a == toRef (allyIdOf ally) && abilityIndex a == 1
  _ -> False

spec :: Spec
spec = describe "spending up to a number of resources" do
  it "cannot initiate without at least one available resource" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      ally <- boundedAlly p
      clearHand p
      offers (\case UseAbility a -> abilitySource a == toRef (allyIdOf ally) && abilityIndex a == 1; _ -> False) `shouldReturn` False
      Ally.allyCounters <$> allyIn "01068" `shouldReturn` 9

  it "cannot stop at zero and may spend just one resource" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      pass
      ally <- boundedAlly p
      clearHand p
      resource <- putInHand p "01005"
      _ <- putInHand p "01089"
      activate ally
      offers isFinishPayment `shouldReturn` False
      choose "spend one" (isPayWith resource)
      choose "stop after one" isFinishPayment
      Ally.allyCounters <$> allyIn "01068" `shouldReturn` 1

  it "can stop early while more resources are available" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      ally <- boundedAlly p
      clearHand p
      energy <- putInHand p "01088"
      _ <- putInHand p "01089"
      activate ally
      choose "spend two" (isPayWith energy)
      choose "stop below the cap" isFinishPayment
      choose "spend both Energy icons" (isLabel $ allocationLabel [Energy, Energy])
      Ally.allyCounters <$> allyIn "01068" `shouldReturn` 2

  it "can spend only one of two generated resources and waste the other" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      changeForm p
      ally <- boundedAlly p
      clearHand p
      energy <- putInHand p "01088"
      _ <- putInHand p "01089"
      activate ally
      choose "generate two" (isPayWith energy)
      choose "finish generating" isFinishPayment
      choose "spend one and waste one" (isLabel $ allocationLabel [Energy])
      Ally.allyCounters <$> allyIn "01068" `shouldReturn` 1

  it "combines hand resources and a resource ability up to three" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      ally <- boundedAlly p
      quincarrier <- putIntoPlay p "08023"
      clearHand p
      energy <- putInHand p "01088"
      activate ally
      choose "spend two from hand" (isPayWith energy)
      choose "generate one with Quincarrier" (isAbilityOfCard quincarrier)
      whenM (offers isFinishPayment) $ choose "finish at three" isFinishPayment
      choose "spend three generated resources" (isLabel $ allocationLabel [Energy, Energy, Wild])
      Ally.allyCounters <$> allyIn "01068" `shouldReturn` 3

  it "wastes overgeneration and cannot increase the modifier above three" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      ally <- boundedAlly p
      clearHand p
      energy <- putInHand p "01088"
      genius <- putInHand p "01089"
      activate ally
      choose "spend Energy" (isPayWith energy)
      choose "spend Genius; its excess is wasted" (isPayWith genius)
      whenM (offers $ isLabelStarting "$label.spendResourceAllocation") $
        choose "choose which three icons were spent" (isLabelStarting "$label.spendResourceAllocation")
      a <- allyIn "01068"
      Ally.allyCounters a `shouldBe` 3
      Ally.getModifiedAttack a `shouldReturn` 5
