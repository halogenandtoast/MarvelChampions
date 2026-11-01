module Marvel.Ally.Allies.ShadowcatKittyPryde where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

shadowcatKittyPryde :: AllyCard ShadowcatKittyPryde
shadowcatKittyPryde = ally ShadowcatKittyPryde Cards.shadowcatKittyPryde (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype ShadowcatKittyPryde = ShadowcatKittyPryde (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ShadowcatKittyPryde where
  getModifiersFor _ (AllyRef you) (ShadowcatKittyPryde a) = pure $ if you == allyId a then [IgnoresGuard, IgnoresPatrol, IgnoresCrisis] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage ShadowcatKittyPryde where
  runMessage msg (ShadowcatKittyPryde attrs) = runQueueT $ case msg of
    _ -> ShadowcatKittyPryde <$> runMessage msg attrs
