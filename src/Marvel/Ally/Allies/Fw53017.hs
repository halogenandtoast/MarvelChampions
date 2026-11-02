{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw53017 where
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
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

import Marvel.FalconWinter.Helpers
fw53017 :: AllyCard Fw53017
fw53017 = ally Fw53017 Cards.fw53017 (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Fw53017 = Fw53017 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53017 where
  getAbilities a@(Fw53017 attrs) =
    [limitedWindowAbility a 1 (W.EntersPlayThis) Response (OwnsThis) (NoCost) (runAbility a 1)]

instance RunMessage Fw53017 where
  runMessage msg a@(Fw53017 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      top <- take 10 <$> getEncounterDeck
      chooseOneSearchedIn you "$label.searchEncounterDeck" [("$label.encounterDeck", map EncounterCard top)] $
        labeled "$label.done" shuffleEncounterDeck : [targeting card (removeFromGame (CardIdRef $ ecCardId card) >> shuffleEncounterDeck >> putEncounterCardIntoPlay you card >> countFalconIcons attrs "readyBirds" you [card]) | card <- top, cardMatch (CardWithType MinionType) card]
      pure a
    WithBoostIcons (isTarget attrs -> True) value counts -> a <$ resolveFalconIcons attrs value counts
    CardSpecific (isTarget attrs -> True) "readyBirds" (fromJSON -> Success (you, n :: Natural)) -> do
      when (n > 0) $ do
        characters <- selectList $ CharacterControlledBy (IdentityWithId you) <> CharacterCanBeReadied
        unless (null characters) $ chooseOne you [targeting (characterRef character) (readyFriendlyCharacter character >> cardSpecific attrs "readyBirds" (you, n - 1)) | character <- characters]
      pure a
    _ -> Fw53017 <$> runMessage msg attrs
