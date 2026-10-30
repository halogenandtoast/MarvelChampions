module Marvel.Rules.NovaMechanicsSpec (spec) where

import Data.Aeson (Result (..), Value (Object), fromJSON, toJSON)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.Damage
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Matchers.Types
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Status
import TestImport

newtype StackingTough = StackingTough (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage StackingTough where
  runMessage msg (StackingTough a) = StackingTough <$> runMessage msg a

stackingOn :: MinionId -> GameAppT ()
stackingOn mid = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (StackingTough EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = MinionEntity (MinionWithId mid)
    , effectModifiers = [CanHaveAnyNumberOfToughStatusCards]
    , effectEnds = Nothing })

minionNow :: MinionId -> GameAppT (Attrs Minion.Minion)
minionNow mid = getsGame (toAttrs . fromMaybe (error "missing minion") . HashMap.lookup mid . gameMinions)

toughCount :: MinionId -> GameAppT Natural
toughCount mid = do
  a <- minionNow mid
  pure $ (if Minion.minionTough a then 1 else 0) + Minion.minionAdditionalTough a

spec :: Spec
spec = describe "Nova engine mechanics" do
  it "doubles generated wilds before declaration, preserving other generated resource types" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let doubled = (lookupPlayerCard "01077") {cdDoubleWildResources = True}
      doubleWildResources (Just doubled) [Physical, Wild, Mental, Wild]
        `shouldBe` [Physical, Wild, Wild, Mental, Wild, Wild]
      doubleWildResources Nothing [Wild, Mental] `shouldBe` [Wild, Mental]

  it "counts neither held nor newly drawn exempt cards while drawing to hand size" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      clearHand p
      held <- putInHand p "28007"
      drawn <- stackDeck p ["28007", "01089", "01088", "01090", "01089", "01088", "01090"]
      resolve [IdentityMessage p DrawToHandLimit]
      hand <- handOf p
      length hand `shouldBe` 8
      hand `shouldSatisfy` elem held
      hand `shouldSatisfy` elem (head drawn)
      length (filter (not . cdDoesNotCountTowardHandSize . getCardDef) hand) `shouldBe` 6

  it "allows only the designated resource from a Civilian's hand to help another payer" do
    tg <- testGame ["quicksilver", "captain-marvel"]
    let helper = player1 tg
        payer = player2 tg
    inGame tg do
      clearHand helper
      everyday <- putInHand helper "28019"
      _ <- putInHand helper "01089"
      getPaymentSourcesFor payer helper Nothing `shouldReturn` [everyday]
      getPaymentResourceAbilities payer helper Nothing `shouldReturn` []
      changeForm helper
      getPaymentSourcesFor payer helper Nothing `shouldReturn` []

  it "retains stacked tough statuses, consuming one per damaging instance and all for piercing" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraRegular
      stackingOn mid
      resolve [GiveStatus (toRef mid) ToughStatus, GiveStatus (toRef mid) ToughStatus, GiveStatus (toRef mid) ToughStatus]
      toughCount mid `shouldReturn` 3
      resolve [MinionMessage mid $ MinionDamaged GameRef (toDamage 0 FromAbility)]
      toughCount mid `shouldReturn` 3
      resolve [MinionMessage mid $ MinionDamaged GameRef (withPiercing $ toDamage 0 FromAbility)]
      toughCount mid `shouldReturn` 3
      resolve [MinionMessage mid $ MinionDamaged GameRef (toDamage 1 FromAbility)]
      toughCount mid `shouldReturn` 2
      Minion.minionDamage <$> minionNow mid `shouldReturn` 0
      resolve [RemoveStatus (toRef mid) ToughStatus]
      toughCount mid `shouldReturn` 1
      resolve [GiveStatus (toRef mid) ToughStatus]
      toughCount mid `shouldReturn` 2
      resolve [MinionMessage mid $ MinionDamaged GameRef (withPiercing $ toDamage 1 FromAbility)]
      toughCount mid `shouldReturn` 0
      Minion.minionDamage <$> minionNow mid `shouldReturn` 1

  it "continues to cap ordinary minions at one tough status" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.hydraRegular
      resolve [GiveStatus (toRef mid) ToughStatus, GiveStatus (toRef mid) ToughStatus]
      toughCount mid `shouldReturn` 1

  it "loads card metadata from saves predating the new constant abilities" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let original = lookupPlayerCard "01077"
          legacy = case toJSON original of
            Object o -> Object $ foldr KeyMap.delete o ["cdDoubleWildResources", "cdDoesNotCountTowardHandSize", "cdCanPayForOthersWithTrait"]
            other -> other
      case fromJSON legacy of
        Error err -> fail err
        Success restored -> restored `shouldBe` original
