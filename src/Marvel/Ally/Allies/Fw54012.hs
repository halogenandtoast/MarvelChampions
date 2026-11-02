{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw54012 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.FalconWinter.Helpers
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

fw54012 :: AllyCard Fw54012
fw54012 = ally Fw54012 Cards.fw54012 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Fw54012 = Fw54012 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw54012 where
  getAbilities a@(Fw54012 attrs) =
    [limitedWindowAbility a 1 (W.EntersPlayThis) Response (OwnsThis <> CharacterExists (CharacterWithTrait T.Shield <> CharacterCanBeReadied)) (NoCost) (runAbility a 1)]

instance RunMessage Fw54012 where
  runMessage msg a@(Fw54012 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      characters <- selectList $ CharacterWithTrait T.Shield <> CharacterCanBeReadied
      chooseOne you [targeting (characterRef character) $ readyFriendlyCharacter character | character <- characters]
      pure a
    _ -> Fw54012 <$> runMessage msg attrs
