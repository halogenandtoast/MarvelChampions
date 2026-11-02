{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.JessicaLuke where
import Marvel.Prelude
import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers
import Marvel.Phase
import Marvel.Support.Types

newtype JLSupport = JLSupport (Attrs Support)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsSupport

jl61002 :: SupportCard JLSupport
jl61002 = support JLSupport Cards.jl61002

jl61008 :: SupportCard JLSupport
jl61008 = support JLSupport Cards.jl61008

jl62007 :: SupportCard JLSupport
jl62007 = support JLSupport Cards.jl62007

jl62018 :: SupportCard JLSupport
jl62018 = supportWith JLSupport Cards.jl62018 $ (usesL .~ 3) . (discardIfNoUsesL .~ True)

jl62019 :: SupportCard JLSupport
jl62019 = support JLSupport Cards.jl62019

instance HasModifiersFor JLSupport where
  getModifiersFor _ target (JLSupport a)
    | toCardCode a == "62019" = do
      characters <- selectList (CharacterControlledBy $ IdentityWithId $ supportController a)
      pure [StatsAtOneRemainingHitPoint 2 | target `elem` map characterRef characters || target `elem` map toRef characters]
  getModifiersFor _ _ _ = pure []

instance HasAbilities JLSupport where
  getAbilities (JLSupport a) = case toCardCode a of
    "61002" -> [limitedWindowAbility a 1 (W.SideSchemeDefeated W.After AnySideScheme) Response OwnsThis ExhaustCost (runAbility a 1)]
    "61008" -> [ability a 1 AlterEgoAction (OwnsThis <> ExtendedCardExists (InDeckOf self $ BasicCardMatches $ CardWithType AllyType))
      (ExhaustCost <> CountersFromSupportCost (SupportNamed "Alias Investigations" <> SupportControlledBy self) 2) (runAbility a 1)]
    "62007" -> [limitedWindowAbility a 1 (W.CharacterDiscardsTough W.After $ CharacterWithTrait T.Defender) Response OwnsThis ExhaustCost (runAbility a 1)]
    "62018" -> [ability a 1 Action (OwnsThis <> CharacterExists (AllyCharacters (AllyWithTrait T.Defender) <> CharacterCanGainTough)) (ExhaustCost <> UseCost) (runAbility a 1)]
    _ -> []
   where self = IdentityWithId $ supportController a

instance RunMessage JLSupport where
  runMessage msg e@(JLSupport a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "61002" -> placeCounters a 2 >> cardSpecific a "finishInvestigation" you
        "61008" -> do
          allCards <- selectList (InDeckOf (IdentityWithId you) $ BasicCardMatches $ CardWithType AllyType)
          cards <- filterM (canPlaySearchedCardWithReduction you 2) allCards
          chooseOne you [targeting c (playSearchedCardWithReduction you 2 c >> shuffleDeck you) | c <- cards]
        "62007" -> for_ windows $ \case
          W.IdentityDiscardedTough p -> ready p
          W.CharacterDiscardedTough character -> readyCharacter character
          _ -> pure ()
        "62018" -> chooseFriendlyTough you (AllyCharacters $ AllyWithTrait T.Defender)
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "finishInvestigation" (fromJSON -> Success (you :: IdentityId)) -> do
      n <- getCountersOn a
      withTheVillainIfAny you $ \villain -> do
        targets <- filterM (fmap (<= n) . getEnemyRemainingHitPoints . EnemyVillainId) (maybeToList villain)
        chooseOne you $ [targeting v $ do
          hp <- getEnemyRemainingHitPoints (EnemyVillainId v)
          removeCounters a hp
          defeatVillain v | v <- targets] <> [labeled "$label.done" $ pure ()]
      pure e
    _ -> JLSupport <$> runMessage msg a
