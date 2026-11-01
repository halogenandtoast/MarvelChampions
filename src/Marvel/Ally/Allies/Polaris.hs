module Marvel.Ally.Allies.Polaris where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats
import Marvel.Status
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

polaris :: AllyCard Polaris
polaris = ally Polaris Cards.polaris (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Polaris = Polaris (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Polaris where
  getAbilities (Polaris a) =
    [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> CharacterExists (CharacterWithTrait Trait.XMen <> CharacterCanGainTough)) NoCost (runAbility a 1)]

instance RunMessage Polaris where
  runMessage msg e@(Polaris attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- selectList (CharacterWithTrait Trait.XMen <> CharacterCanGainTough)
      chooseOne you [targeting (characterRef character) (giveStatus character ToughStatus) | character <- characters]
      pure e
    _ -> Polaris <$> runMessage msg attrs
