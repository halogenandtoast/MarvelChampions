module Marvel.Ally.Allies.CaptainAmericaSteveRogers (
  captainAmericaSteveRogers,
  CaptainAmericaSteveRogers (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Id
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Avenger))

-- | Toughness (in the def).
captainAmericaSteveRogers :: AllyCard CaptainAmericaSteveRogers
captainAmericaSteveRogers =
  ally CaptainAmericaSteveRogers Cards.captainAmericaSteveRogers (Thw 2, 1) (Atk 2, 1) (HP 4)

newtype CaptainAmericaSteveRogers = CaptainAmericaSteveRogers (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | In hand, his cost is 1 less for each Avenger character his player controls: their
-- identity, in the form it is in, and their allies.
instance HasModifiersFor CaptainAmericaSteveRogers where
  getModifiersFor _ (CardIdRef cid) (CaptainAmericaSteveRogers a) | cid == CardId (unAllyId $ allyId a) = do
    let you = allyController a
    identity <- selectListCount (IdentityWithId you <> IdentityWithTrait Avenger)
    allies <- selectListCount (AllyControlledBy (IdentityWithId you) <> AllyWithTrait Avenger)
    pure [ResourceCostReduction (identity + allies)]
  getModifiersFor _ _ _ = pure []

instance RunMessage CaptainAmericaSteveRogers where
  runMessage msg (CaptainAmericaSteveRogers attrs) = CaptainAmericaSteveRogers <$> runMessage msg attrs
