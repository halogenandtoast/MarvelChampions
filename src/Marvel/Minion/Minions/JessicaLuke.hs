{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Minion.Minions.JessicaLuke where
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
import Marvel.Attack (Attack (..))
import Marvel.Ally.Types (Ally, Field (AllyController))
import Marvel.Boost qualified as B
import Marvel.Phase
import Marvel.Minion.Types
import Marvel.Minion.Runner ()

newtype JLMinion = JLMinion (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsMinion

jl61031 :: MinionCard JLMinion
jl61031 = minion JLMinion Cards.jl61031 (Sch 0) (Atk 0) (HP 6)

jl62029 :: MinionCard JLMinion
jl62029 = minion JLMinion Cards.jl62029 (Sch 2) (Atk 1) (HP 5)

jl62032 :: MinionCard JLMinion
jl62032 = minion JLMinion Cards.jl62032 (Sch 1) (Atk 2) (HP 4)

jl62033 :: MinionCard JLMinion
jl62033 = minion JLMinion Cards.jl62033 (Sch 1) (Atk 2) (HP 3)

instance HasModifiersFor JLMinion where
  getModifiersFor _ target (JLMinion a)
    | toCardCode a == "61031", target == toRef a = do
      friendlies <- selectList (CharacterControlledBy AnyIdentity)
      enemies <- selectList (NotEnemy $ EnemyWithId $ EnemyMinionId $ minionId a)
      let enemyCharacters = map (\case EnemyVillainId v -> VillainCharacter v; EnemyMinionId m -> MinionCharacter m; EnemyAttachmentId x -> AttachmentCharacter x) enemies
      attacks <- traverse characterAttackValue (friendlies <> enemyCharacters)
      thwarts <- traverse getCharacterThwart friendlies
      schemes <- traverse getEnemyScheme enemies
      pure [BaseAttack $ maximum (0:attacks), BaseScheme $ maximum (0:thwarts <> schemes)]
  getModifiersFor _ _ _ = pure []

instance HasAbilities JLMinion where
  getAbilities (JLMinion a) = case toCardCode a of
    "62029" -> [limited (PerPhase 1) $ limitedWindowAbility a 1 (W.EnemyAttackedAnyTarget W.After self) ForcedResponse NoCriteria NoCost (runAbility a 1)]
    "62032" -> [limitedWindowAbility a 1 (W.MinionHasSchemed W.After $ MinionWithId $ minionId a) ForcedResponse NoCriteria NoCost (runAbility a 1)]
    "62033" -> [limitedWindowAbility a 1 (W.WindowOneOf [W.IdentityTargetsEnemy W.When AnyIdentity self, W.AllyAttacked W.When AnyAlly self]) ForcedInterrupt NoCriteria NoCost (runAbility a 1)]
    _ -> []
   where self = EnemyWithId $ EnemyMinionId $ minionId a

instance RunMessage JLMinion where
  runMessage msg e@(JLMinion a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "62029" -> do
          attack <- getEnemyAttack (EnemyMinionId $ minionId a)
          for_ attack $ \details -> cardSpecific a "cottonRepeat" (attackCharacter details, 2 :: Natural)
        "62032" -> stunIdentity (minionEngagedIdentity a)
        "62033" -> do
          let players = [you | W.IdentityAttackTarget you _ <- windows]
          allyPlayers <- catMaybes <$> traverse getAllyControllerId [aid | W.AllyAttack aid _ <- windows]
          for_ (take 1 $ players <> allyPlayers) $ \you ->
            discardTopOfEncounterDeckThen 1 $ \cards ->
              when (any (elem B.Star . cdBoostIcons . getCardDef) cards) $ dealMinionAsEncounterCard (minionId a) you
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "cottonRepeat" (fromJSON -> Success (target :: CharacterId, left :: Natural)) -> do
      present <- selectAny (CharacterWithId target)
      active <- selectAny (MinionWithId $ minionId a)
      when (left > 0 && present && active) $ do
        case target of
          AllyCharacter ally -> getAllyControllerId ally >>= traverse_ (\owner -> enemyAttacksAlly (minionId a) owner ally)
          IdentityCharacter you -> enemyAttacks (minionId a) you
          _ -> pure ()
        cardSpecific a "cottonRepeat" (target,left-1)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      case toCardCode a of
        "62029" -> selectOne (IdentityWithTitle "Luke Cage") >>= traverse_ (dealBoostAsEncounterCard a)
        "62032" -> getBoostedPlayer (toEnemy enemy) >>= stunIdentity
        _ -> pure ()
      pure e
    _ -> JLMinion <$> runMessage msg a
