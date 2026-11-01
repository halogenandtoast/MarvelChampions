module Marvel.Minion.Minions.AgeOfApocalypseUnus where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier (HasModifiersFor)
import Marvel.Query
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (placeGenePoolThreat)
import Marvel.Stats

newtype AoaUnusMinion = AoaUnusMinion (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

infiniteHunter :: MinionCard AoaUnusMinion
infiniteHunter = minion AoaUnusMinion Cards.infiniteHunter (Sch 2) (Atk 3) (HP 4)

instance RunMessage AoaUnusMinion where
  runMessage msg e@(AoaUnusMinion a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      allies <- selectList (AllyControlledBy (IdentityWithId you))
      unless (null allies) $ chooseOrRunOne you [targeting (AllyRef aid) (damageCharacter aid a $ toDamage 3 FromAbility) | aid <- allies]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      chooseOne you
        [ labeled "$label.aoaPlaceTwoGenePoolThreat" (placeGenePoolThreat 2)
        , labeled "$label.aoaActivationBonusTwo" (enemyActivationBonus enemy 2)
        ]
      pure e
    _ -> AoaUnusMinion <$> runMessage msg a
