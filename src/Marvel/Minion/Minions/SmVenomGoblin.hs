module Marvel.Minion.Minions.SmVenomGoblin where

import Marvel.Prelude
import Marvel.Ability.Types
import Data.Aeson ()
import Marvel.Game.Source
import Marvel.MainScheme.MainSchemes.SmVenomGoblin qualified as Goblin
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Keyword
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner ()

newtype SmVenomGoblinMinion = SmVenomGoblinMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSymbioticBerserker :: MinionCard SmVenomGoblinMinion
smSymbioticBerserker = minion SmVenomGoblinMinion Cards.smSymbioticBerserker (Sch 0) (Atk 3) (HP 5)

smSymbioticMonstrosity :: MinionCard SmVenomGoblinMinion
smSymbioticMonstrosity = minion SmVenomGoblinMinion Cards.smSymbioticMonstrosity (Sch 2) (Atk 2) (HP 6)

smSymbioticThrall :: MinionCard SmVenomGoblinMinion
smSymbioticThrall = minion SmVenomGoblinMinion Cards.smSymbioticThrall (Sch 1) (Atk 2) (HP 4)

instance HasModifiersFor SmVenomGoblinMinion where
  getModifiersFor _ target (SmVenomGoblinMinion attrs)
    | target == toRef attrs = do
        infested <- (> 0) <$> Goblin.symbioteEnvironmentCount
        pure $ if not infested then [] else case toCardCode attrs of
          "27121" -> [KeywordModifier Quickstrike]
          "27122" -> [HitPointModifier 3]
          "27123" -> [KeywordModifier Patrol]
          _ -> []
  getModifiersFor _ _ _ = pure []

instance RunMessage SmVenomGoblinMinion where
  runMessage msg e@(SmVenomGoblinMinion attrs) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      case toCardCode attrs of
        "27121" -> Goblin.chooseGliderScheme True you "goblinBerserkerBoost"
        "27122" -> whenM (selectAny $ EnemyWithId enemy <> AttackingEnemy) $ makeAttackIndirect enemy
        "27123" -> do
          gliders <- getGliderSchemes
          schemes <- selectList MainScheme
          for_ [sid | SchemeMainSchemeId sid <- schemes, sid `notElem` gliders] $ \sid -> placeThreat sid 1
        _ -> pure ()
      pure e
    _ -> SmVenomGoblinMinion <$> runMessage msg attrs
