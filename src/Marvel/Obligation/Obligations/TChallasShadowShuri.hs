module Marvel.Obligation.Obligations.TChallasShadowShuri where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Def (CardMatcher (AnyCard))
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W

tChallasShadowShuri :: ObligationCard TChallasShadowShuri
tChallasShadowShuri = obligation (TChallasShadowShuri . (`With` (4 :: Natural))) Cards.tChallasShadowShuri
newtype TChallasShadowShuri = TChallasShadowShuri (Attrs Obligation `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsObligation TChallasShadowShuri where
  toObligationAttrs (TChallasShadowShuri (attrs `With` _)) = attrs
  obligationDisplayCounters (TChallasShadowShuri (_ `With` n)) = n
instance HasAbilities TChallasShadowShuri where
  getAbilities u@(TChallasShadowShuri (attrs `With` _)) = case obligationOwner attrs of
    Nothing -> []
    Just you -> [limitedWindowAbility u 1 (W.WindowOneOf [W.IdentityAttackResolved (IdentityWithId you), W.HeroThwarted W.After (IdentityWithId you), W.HeroDefended W.After (IdentityWithId you) AnyEnemy]) ForcedResponse NoCriteria NoCost (runAbility u 1)]
instance HasModifiersFor TChallasShadowShuri where
  getModifiersFor _ (IdentityRef you) (TChallasShadowShuri (attrs `With` _)) | Just you == obligationOwner attrs = pure [AdditionalResourceCost AnyCard 1]
  getModifiersFor _ _ _ = pure []
instance RunMessage TChallasShadowShuri where
  runMessage msg u@(TChallasShadowShuri (attrs `With` n)) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure u
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      when (n == 1) $ addToVictoryDisplay attrs
      pure $ TChallasShadowShuri (attrs `With` subtractNatural 1 n)
    _ -> TChallasShadowShuri . (`With` n) <$> runMessage msg attrs
