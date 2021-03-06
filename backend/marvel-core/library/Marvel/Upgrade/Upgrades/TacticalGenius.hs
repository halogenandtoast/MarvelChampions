module Marvel.Upgrade.Upgrades.TacticalGenius
  ( tacticalGenius
  , TacticalGenius(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

tacticalGenius :: UpgradeCard TacticalGenius
tacticalGenius = upgrade TacticalGenius Cards.tacticalGenius

newtype TacticalGenius = TacticalGenius UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities TacticalGenius where
  getAbilities _ = []

instance RunMessage TacticalGenius where
  runMessage msg u@(TacticalGenius attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> thwartGuard u $ do
      modifiers <- getModifiers attrs
      let dmg = if LastSpecial `elem` modifiers then 2 else 1
      msgs <- choiceMessages (upgradeController attrs)
        $ RemoveThreat (toSource attrs) dmg ThwartableScheme
      pushAll msgs
      pure u
    _ -> TacticalGenius <$> runMessage msg attrs
