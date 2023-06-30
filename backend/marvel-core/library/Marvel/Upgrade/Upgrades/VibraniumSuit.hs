module Marvel.Upgrade.Upgrades.VibraniumSuit (
  vibraniumSuit,
  VibraniumSuit (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

vibraniumSuit :: UpgradeCard VibraniumSuit
vibraniumSuit = upgrade VibraniumSuit Cards.vibraniumSuit

newtype VibraniumSuit = VibraniumSuit (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasAbilities VibraniumSuit where
  getAbilities _ = []

instance RunMessage VibraniumSuit where
  runMessage msg u@(VibraniumSuit attrs) = case msg of
    RanAbility target 1 _ _ | isTarget attrs target -> do
      let ident = upgradeController attrs
      stunned <- selectAny (IdentityWithId ident <> StunnedIdentity)
      if stunned
        then push (IdentityMessage ident IdentityRemoveStunned)
        else do
          modifiers <- getModifiers attrs
          sustainedDamage <-
            selectCount
              SustainedDamage
              (IdentityWithId $ upgradeController attrs)
          let
            dmg =
              min sustainedDamage $
                if LastSpecial `elem` modifiers then 2 else 1
          damageMsgs <-
            choiceMessages (upgradeController attrs) $
              ChooseDamage
                (toRef attrs)
                (toDamage dmg FromAbility)
                AttackableEnemy
          healMsgs <-
            choiceMessages (upgradeController attrs) $
              Heal (IdentityCharacter $ upgradeController attrs) dmg
          pushAll $ damageMsgs <> healMsgs
      pure u
    _ -> VibraniumSuit <$> runMessage msg attrs
