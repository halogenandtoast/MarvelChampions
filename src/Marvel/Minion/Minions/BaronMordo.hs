module Marvel.Minion.Minions.BaronMordo (
  baronMordo,
  BaronMordo (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Events.MagicBlast (printedResourceEffects)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window qualified as W

baronMordo :: MinionCard BaronMordo
baronMordo = minion BaronMordo Cards.baronMordo (Sch 2) (Atk 2) (HP 5)

newtype BaronMordo = BaronMordo (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BaronMordo where
  getAbilities (BaronMordo a) =
    [ windowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId $ EnemyMinionId $ minionId a) You) ForcedInterrupt NoCost $
        runAbility a 1
    ]

-- | When he attacks you, discard the top card of your deck: its printed [physical]
-- stuns you, [energy] deals you 2 damage, [mental] confuses you, [wild] all three.
instance RunMessage BaronMordo where
  runMessage msg e@(BaronMordo attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [ident | W.EnemyAttack _ ident <- windows] \ident -> discardFromDeck ident 1 attrs
      pure e
    WithDiscarded (isTarget attrs -> True) FromDeck (onlyPlayerCards -> cards) -> do
      for_ [(card, ident) | card <- cards, Just ident <- [pcOwner card]] \(card, ident) ->
        sequence_ $
          printedResourceEffects
            (printedResources $ getCardDef card)
            (stunIdentity ident)
            (damageCharacter ident attrs (toDamage 2 FromAbility))
            (confuseIdentity ident)
      pure e
    _ -> BaronMordo <$> runMessage msg attrs
