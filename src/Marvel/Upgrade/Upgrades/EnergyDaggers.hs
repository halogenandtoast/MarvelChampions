module Marvel.Upgrade.Upgrades.EnergyDaggers (
  energyDaggers,
  EnergyDaggers (..),
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
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
  runMessage msg u@(EnergyDaggers attrs) = runQueueT $ case msg of
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
      -- dealt as one effect, as any damage to an enemy: its windows open and what adds
      -- to this card's damage adds to each
      chooseOne
        ident
        [ targeting ident' $
          dealDamageToEach (toEnemy villain : map toEnemy (HashMap.findWithDefault [] ident' minionMap)) attrs dmg
        | ident' <- players
        ]
      pure u
    _ -> EnergyDaggers <$> runMessage msg attrs
