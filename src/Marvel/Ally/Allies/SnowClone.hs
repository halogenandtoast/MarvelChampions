module Marvel.Ally.Allies.SnowClone where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Stats
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers
import Marvel.Ally.Runner

snowClone :: AllyCard SnowClone
snowClone = ally (SnowClone . (`With` [])) Cards.snowClone (Thw 0, 0) (Atk 2, 1) (HP 2)

newtype SnowClone = SnowClone (Attrs Ally `With` [EnemyId])
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAlly SnowClone where
  allyHasThwart _ = False
  toAllyAttrs (SnowClone (attrs `With` _)) = attrs
instance HasModifiersFor SnowClone where
  getModifiersFor _ target (SnowClone (attrs `With` enemies)) | target == toRef attrs =
    pure $ CannotHaveAttachedPlayerCards : [AttackConsequentialDamageReductionAgainst e 1 | e <- enemies]
  getModifiersFor _ _ _ = pure []
instance RunMessage SnowClone where
  runMessage msg (SnowClone (attrs `With` enemies)) = runQueueT $ case msg of
    CheckWindows ws | targets@(_:_) <- [enemy | W.Window W.When (W.AllyAttack aid enemy) <- ws, aid == allyId attrs] -> do
      frozen <- filterM (enemyMatches frostbitten) targets
      updated <- runMessage msg attrs
      pure $ SnowClone (updated `With` frozen)
    _ -> (\updated -> SnowClone (updated `With` enemies)) <$> runMessage msg attrs
