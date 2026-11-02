module Marvel.Support.Supports.DoraMilajeShuri where
import Marvel.Prelude
import Marvel.Card.Id
import Marvel.Id
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T
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
import Marvel.Support.Types

doraMilajeShuri :: SupportCard DoraMilajeShuri
doraMilajeShuri = support DoraMilajeShuri Cards.doraMilajeShuri
newtype DoraMilajeShuri = DoraMilajeShuri (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage DoraMilajeShuri where
  runMessage msg u@(DoraMilajeShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      allies <- selectList (AllyWithTrait T.DoraMilaje)
      chooseOne you [targeting aid (resolveAs you (runAbility aid 2) >> heal aid 1) | aid <- allies]
      pure u
    _ -> DoraMilajeShuri <$> runMessage msg attrs

instance HasAbilities DoraMilajeShuri where
  getAbilities u@(DoraMilajeShuri _attrs) = [ability u 1 Action (OwnsThis <> AllyExists (AllyWithTrait T.DoraMilaje)) ExhaustCost (runAbility u 1)]

instance HasModifiersFor DoraMilajeShuri where
  getModifiersFor _ (CardIdRef cid) (DoraMilajeShuri attrs) | cid == CardId (unSupportId $ supportId attrs) = do
    wakandan <- identityMatches (IdentityWithTrait T.Wakanda) (supportController attrs)
    pure [IgnoresResourceCost | wakandan]
  getModifiersFor _ _ _ = pure []
