module Marvel.Ally.Allies.IronManWarMachine (ironManWarMachine, IronManWarMachine (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Message.Lifted ()
import Marvel.Trait (Trait (Tech))
import Marvel.Window

ironManWarMachine :: AllyCard IronManWarMachine
ironManWarMachine = ally IronManWarMachine Cards.ironManWarMachine (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype IronManWarMachine = IronManWarMachine (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities IronManWarMachine where
  getAbilities a = [limitedWindowAbility a 1 EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage IronManWarMachine where
  runMessage msg a@(IronManWarMachine attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      search ident (SearchIdentityDeckAndDiscardPile ident)
        (CardWithType UpgradeType <> CardWithTrait Tech) SearchDrawOne ShuffleBackIn
      pure a
    _ -> IronManWarMachine <$> runMessage msg attrs
