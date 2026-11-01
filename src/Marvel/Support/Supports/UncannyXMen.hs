module Marvel.Support.Supports.UncannyXMen where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait qualified as Trait

uncannyXMen :: SupportCard UncannyXMen
uncannyXMen = support UncannyXMen Cards.uncannyXMen

newtype UncannyXMen = UncannyXMen (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor UncannyXMen where
  getModifiersFor _ (AllyRef aid) (UncannyXMen attrs) = do
    yours <- selectAny (AllyWithId aid <> AllyControlledBy (IdentityWithId $ supportController attrs))
    if not yours then pure [] else do
      xmen <- selectAny (AllyWithId aid <> AllyWithTrait Trait.XMen)
      pure [HitPointModifier 1 | xmen]
  getModifiersFor _ (IdentityRef you) (UncannyXMen attrs) | you == supportController attrs = do
    identityIsXMen <- selectAny (IdentityWithId you <> IdentityWithTrait Trait.XMen)
    if not identityIsXMen then pure [] else do
      -- Filter by control before asking traits. CharacterWithTrait searches
      -- villains and minions too, which is unnecessary for this condition.
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      allXMen <- allM (\aid -> selectAny $ AllyWithId aid <> AllyWithTrait Trait.XMen) allies
      pure [ResourceCostReductionFor (CardWithType AllyType <> CardWithTrait Trait.XMen) 1 | allXMen]
  getModifiersFor _ _ _ = pure []

instance RunMessage UncannyXMen where
  runMessage msg (UncannyXMen attrs) = runQueueT $ UncannyXMen <$> runMessage msg attrs
