module Marvel.Minion.Minions.Abomination (
  abomination,
  Abomination (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Resource
import Marvel.Window

abomination :: MinionCard Abomination
abomination = minion Abomination Cards.abomination (Sch 2) (Atk 3) (HP 6)

newtype Abomination = Abomination (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Forced Response: After Abomination attacks you, discard the top card of your
-- deck. If a [physical] resource was discarded this way, take 2 damage." The player
-- attacked is "you", whoever resolves it.
instance HasAbilities Abomination where
  getAbilities (Abomination a) =
    [ windowAbility
        a
        1
        (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity)
        ForcedResponse
        NoCost
        $ RunAbility (toTarget a) 1
    ]

attacked :: [WindowType] -> Maybe IdentityId
attacked = \case
  EnemyAttack _ ident : _ -> Just ident
  _ : ws -> attacked ws
  [] -> Nothing

instance RunMessage Abomination where
  runMessage msg e@(Abomination attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 (attacked -> Just ident) _ -> do
      discardFromDeck ident 1 attrs
      pure e
    WithDiscarded (isTarget attrs -> True) FromDeck (onlyPlayerCards -> cards) -> do
      for_ [ident | card <- cards, cardMatch (CardWithResource Physical) card, Just ident <- [pcOwner card]] \ident ->
        damageIdentity ident attrs (toDamage 2 FromAbility)
      pure e
    _ -> Abomination <$> runMessage msg attrs
