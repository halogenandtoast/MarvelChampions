module Marvel.Ally.Allies.Randall where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Payment
import Marvel.Question
import Marvel.Resource
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted hiding (ExhaustedAlly)
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner hiding (ExhaustedAlly)

randall :: AllyCard Randall
randall = ally Randall Cards.randall (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype Randall = Randall (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Randall where
  getAbilities a@(Randall attrs) =
    [limitedAbility a 1 (PerPhase 1) Action
      (OwnsThis <> AnyCriteria [AllyExists (AllyWithId (allyId attrs) <> ExhaustedAlly <> AllyCanReady), CharacterExists (AllyCharacters (AllyWithId $ allyId attrs) <> healableCharacter)])
      (DiscardMatchingHandCardCost $ CardWithType ResourceType <> if allyExhausted attrs then AnyCard else CardWithResource Energy) (runAbility a 1)]

instance RunMessage Randall where
  runMessage msg a@(Randall attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      ready (allyId attrs)
      when (any (elem Energy . printedResources . getCardDef) $ paymentDiscards payment) $ heal (allyId attrs) 1
      pure a
    _ -> Randall <$> runMessage msg attrs
