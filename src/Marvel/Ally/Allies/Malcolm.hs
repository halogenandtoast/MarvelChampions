module Marvel.Ally.Allies.Malcolm where

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

malcolm :: AllyCard Malcolm
malcolm = ally Malcolm Cards.malcolm (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Malcolm = Malcolm (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Malcolm where
  getAbilities a@(Malcolm attrs) =
    [limitedAbility a 1 (PerPhase 1) Action
      (OwnsThis <> AnyCriteria [AllyExists (AllyWithId (allyId attrs) <> ExhaustedAlly <> AllyCanReady), CharacterExists (AllyCharacters (AllyWithId $ allyId attrs) <> healableCharacter)])
      (DiscardMatchingHandCardCost $ CardWithType ResourceType <> if allyExhausted attrs then AnyCard else CardWithResource Physical) (runAbility a 1)]

instance RunMessage Malcolm where
  runMessage msg a@(Malcolm attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      ready (allyId attrs)
      when (any (elem Physical . printedResources . getCardDef) $ paymentDiscards payment) $ heal (allyId attrs) 1
      pure a
    _ -> Malcolm <$> runMessage msg attrs
