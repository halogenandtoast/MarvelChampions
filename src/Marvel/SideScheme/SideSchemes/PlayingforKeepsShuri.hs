module Marvel.SideScheme.SideSchemes.PlayingforKeepsShuri where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

playingforKeepsShuri :: SideSchemeCard PlayingforKeepsShuri
playingforKeepsShuri = sideScheme PlayingforKeepsShuri Cards.playingforKeepsShuri (PerPlayer 3)
newtype PlayingforKeepsShuri = PlayingforKeepsShuri (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage PlayingforKeepsShuri where
  runMessage msg u@(PlayingforKeepsShuri attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [enemy | W.EnemyActivation enemy _ <- windows] dealBoost
      pure u
    _ -> PlayingforKeepsShuri <$> runMessage msg attrs

instance HasAbilities PlayingforKeepsShuri where
  getAbilities u@(PlayingforKeepsShuri _attrs) = [limitedWindowAbility u 1 (W.EnemyActivates W.When AnyEnemy AnyIdentity) ForcedInterrupt NoCriteria NoCost (runAbility u 1)]

instance HasModifiersFor PlayingforKeepsShuri where
  getModifiersFor _ (IdentityRef _) _ = pure [HandSizeModifier 1]
  getModifiersFor _ _ _ = pure []

instance IsSideScheme PlayingforKeepsShuri where
  toSideSchemeAttrs (PlayingforKeepsShuri attrs) = attrs
  sideSchemeAbilities = getAbilities
