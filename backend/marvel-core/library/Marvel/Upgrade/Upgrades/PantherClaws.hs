module Marvel.Upgrade.Upgrades.PantherClaws (
  pantherClaws,
  PantherClaws (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

pantherClaws :: UpgradeCard PantherClaws
pantherClaws = upgrade PantherClaws Cards.pantherClaws

newtype PantherClaws = PantherClaws (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasAbilities PantherClaws where
  getAbilities _ = []

instance RunMessage PantherClaws where
  runMessage msg u@(PantherClaws attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      stunned <- selectAny $ IdentityWithId ident <> StunnedIdentity
      if stunned
        then push (IdentityMessage ident IdentityRemoveStunned)
        else do
          modifiers <- getModifiers attrs
          let dmg = if LastSpecial `elem` modifiers then 4 else 2
          msgs <-
            choiceMessages (upgradeController attrs) $
              ChooseDamage (toRef attrs) (toDamage dmg FromAbility) (AttackableEnemy <> NotEnemy (EnemyIs Cards.killmonger))
          pushAll msgs
      pure u
    _ -> PantherClaws <$> runMessage msg attrs
