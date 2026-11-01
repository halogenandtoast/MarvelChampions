module Marvel.SideScheme.SideSchemes.LayTheTrap where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability.Types qualified as Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

layTheTrap :: SideSchemeCard LayTheTrap
layTheTrap = sideScheme LayTheTrap Cards.layTheTrap (PerPlayer 3)

newtype LayTheTrap = LayTheTrap (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage LayTheTrap where
  runMessage msg e@(LayTheTrap a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- getDefeatedBy a
      n <- fromIntegral <$> fromGameValue (PerPlayer 5)
      enemies <- selectList VillainEnemy
      chooseOne you [targeting (enemyRef enemy) $ dealDamage enemy (IdentityRef you) (toDamage n FromAbility) | enemy <- enemies]
      pure e
    _ -> LayTheTrap <$> runMessage msg a
