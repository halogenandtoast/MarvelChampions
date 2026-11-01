module Marvel.Ally.Allies.Jocasta where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Query
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Upgrade.Cards qualified as Upgrades ()
import Marvel.Window qualified as W
import Marvel.Stats ()
import Marvel.Hp ()
jocasta :: AllyCard Jocasta
jocasta = ally Jocasta Cards.jocasta (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype Jocasta = Jocasta (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Jocasta where
  getAbilities (Jocasta a) =
    [ limitedWindowAbility a 1 W.EntersPlayThis Response
        (OwnsThis <> ExtendedCardExists (InDiscardOf (IdentityWithId $ allyController a) $ BasicCardMatches $ CardWithType EventType <> CardWithTrait Trait.Defense))
        NoCost (runAbility a 1)
    ]

instance RunMessage Jocasta where
  runMessage msg a@(Jocasta attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithType EventType <> CardWithTrait Trait.Defense)
      unless (null cards) $ chooseOne you [targeting c (attachPlayableEventToAlly (allyId attrs) c) | c <- cards]
      pure a
    _ -> Jocasta <$> runMessage msg attrs
