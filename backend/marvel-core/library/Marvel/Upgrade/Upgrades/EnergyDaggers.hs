module Marvel.Upgrade.Upgrades.EnergyDaggers
  ( energyDaggers
  , EnergyDaggers(..)
  ) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Attrs
import Marvel.Upgrade.Cards qualified as Cards

energyDaggers :: UpgradeCard EnergyDaggers
energyDaggers = upgrade EnergyDaggers Cards.energyDaggers

newtype EnergyDaggers = EnergyDaggers UpgradeAttrs
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities EnergyDaggers where
  getAbilities (EnergyDaggers a) =
    [ability a 1 Special Never NoCost $ RunAbility (toTarget a) 1]

instance RunMessage EnergyDaggers where
  runMessage msg u@(EnergyDaggers attrs) = case msg of
    RanAbility target 1 _ | isTarget attrs target -> do
      players <- getPlayers
      villain <- selectJust ActiveVillain
      minionMap <-
        HashMap.fromList
          <$> traverse
                (\iid -> (iid, )
                  <$> selectList (MinionEngagedWith $ IdentityWithId iid)
                )
                players
      modifiers <- getModifiers attrs
      let dmg = if LastSpecial `elem` modifiers then 2 else 1
      u <$ chooseOne
        (upgradeController attrs)
        [ TargetLabel
            (IdentityTarget ident)
            [ Run
              $ VillainMessage villain (VillainDamaged (toSource attrs) dmg)
              : map
                  (\minionId -> MinionMessage
                    minionId
                    (MinionDamaged (toSource attrs) dmg)
                  )
                  (HashMap.findWithDefault [] ident minionMap)
            ]
        | ident <- players
        ]
    _ -> EnergyDaggers <$> runMessage msg attrs
