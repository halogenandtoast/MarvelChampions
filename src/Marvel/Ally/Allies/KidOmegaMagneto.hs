module Marvel.Ally.Allies.KidOmegaMagneto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types qualified as R
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

kidOmegaMagneto :: AllyCard KidOmegaMagneto
kidOmegaMagneto = ally KidOmegaMagneto Cards.kidOmegaMagneto (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype KidOmegaMagneto = KidOmegaMagneto (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities KidOmegaMagneto where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage KidOmegaMagneto where
  runMessage msg e@(KidOmegaMagneto attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you
        [labeled "$label.kidOmegaDamage" $ payCostThenWithPayment you attrs (ResourceCost $ Just R.Energy) attrs "paidDamage" ()
        ,labeled "$label.kidOmegaThreat" $ payCostThenWithPayment you attrs (ResourceCost $ Just R.Mental) attrs "paidThreat" ()]
      pure e
    CardSpecific (isTarget attrs -> True) "paidDamage" _ -> do
      enemies <- selectList AnyEnemy
      dealDamageToEach enemies attrs (toDamage 1 FromAbility)
      pure e
    CardSpecific (isTarget attrs -> True) "paidThreat" _ -> do
      schemes <- selectList AnyScheme
      for_ schemes $ \scheme -> removeThreat (allyController attrs) attrs 1 (SchemeWithId scheme)
      pure e
    _ -> KidOmegaMagneto <$> runMessage msg attrs
