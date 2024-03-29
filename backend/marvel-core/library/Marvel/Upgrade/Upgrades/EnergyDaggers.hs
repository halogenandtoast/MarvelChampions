module Marvel.Upgrade.Upgrades.EnergyDaggers (
  energyDaggers,
  EnergyDaggers (..),
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

energyDaggers :: UpgradeCard EnergyDaggers
energyDaggers = upgrade EnergyDaggers Cards.energyDaggers

newtype EnergyDaggers = EnergyDaggers (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EnergyDaggers where
  getAbilities (EnergyDaggers a) =
    [ability a 1 Special Never NoCost $ RunAbility (toTarget a) 1]

instance RunMessage EnergyDaggers where
  runMessage msg u@(EnergyDaggers attrs) = case msg of
    RanAbility ident target 1 _ _ | isTarget attrs target -> do
      players <- getPlayers
      villain <- selectJust ActiveVillain
      minionMap <-
        HashMap.fromList
          <$> traverse
            ( \iid ->
                (iid,)
                  <$> selectList (MinionEngagedWith $ IdentityWithId iid)
            )
            players
      modifiers <- getModifiers attrs
      let
        dmg =
          toDamage (if LastSpecial `elem` modifiers then 2 else 1) FromAbility
      chooseOne
        ident
        [ TargetLabel
          (toRef ident')
          [ Run $
              VillainMessage villain (VillainDamaged (toSource attrs) dmg)
                : map
                  ( \minionId ->
                      MinionMessage
                        minionId
                        (MinionDamaged (toSource attrs) dmg)
                  )
                  (HashMap.findWithDefault [] ident' minionMap)
          ]
        | ident' <- players
        ]
      pure u
    _ -> EnergyDaggers <$> runMessage msg attrs
