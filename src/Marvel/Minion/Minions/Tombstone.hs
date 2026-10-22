module Marvel.Minion.Minions.Tombstone (
  tombstone,
  Tombstone (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Damage
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Projection
import Marvel.Resource
import Marvel.Window

tombstone :: MinionCard Tombstone
tombstone = minion Tombstone Cards.tombstone (Sch 2) (Atk 3) (HP 9)

newtype Tombstone = Tombstone (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: after Tombstone attacks and damages you, discard a [mental]
-- or a [physical] resource from your hand, if able.
instance HasAbilities Tombstone where
  getAbilities (Tombstone a) =
    [ windowAbility a 1 (TookDamage AnyIdentity FromAttack (EnemyWithId $ EnemyMinionId $ minionId a)) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Tombstone where
  runMessage msg e@(Tombstone attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [IdentityTookDamage you _ _] _ -> do
      cards <-
        projectMap
          PlayerIdentityHand
          (filter (cardMatch $ CardMatchesAny [CardWithResource Mental, CardWithResource Physical]) . unHand)
          you
      unless (null cards) $
        chooseOnePrompted you "$label.chooseCardToDiscard" [targeting card (discardCard card) | card <- cards]
      pure e
    _ -> Tombstone <$> runMessage msg attrs
