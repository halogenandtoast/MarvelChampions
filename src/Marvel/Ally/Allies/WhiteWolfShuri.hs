module Marvel.Ally.Allies.WhiteWolfShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.BlackPantherShuri.Helpers ()

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Ally.Runner

whiteWolfShuri :: AllyCard WhiteWolfShuri
whiteWolfShuri = ally WhiteWolfShuri Cards.whiteWolfShuri (Thw 2, 2) (Atk 2, 0) (HP 3)
newtype WhiteWolfShuri = WhiteWolfShuri (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage WhiteWolfShuri where
  runMessage msg u@(WhiteWolfShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList MainScheme
      chooseOrRunOne you [targeting (schemeRef scheme) (placeThreatOn attrs scheme 1) | scheme <- schemes]
      pure u
    _ -> WhiteWolfShuri <$> runMessage msg attrs

instance HasAbilities WhiteWolfShuri where
  getAbilities u@(WhiteWolfShuri attrs) = [limitedWindowAbility u 1 (W.AllyAttacked W.After (AllyWithId $ allyId attrs) AnyEnemy) ForcedResponse NoCriteria NoCost (runAbility u 1)]
