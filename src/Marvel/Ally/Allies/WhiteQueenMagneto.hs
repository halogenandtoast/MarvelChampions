module Marvel.Ally.Allies.WhiteQueenMagneto where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Identity.Types ()

import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

whiteQueenMagneto :: AllyCard WhiteQueenMagneto
whiteQueenMagneto = ally WhiteQueenMagneto Cards.whiteQueenMagneto (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype WhiteQueenMagneto = WhiteQueenMagneto (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WhiteQueenMagneto where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> AnyCriteria [CharacterExists (CharacterWithStatus status) | status <- [StunnedStatus, ConfusedStatus, ToughStatus]]) NoCost (runAbility a 1)]

instance RunMessage WhiteQueenMagneto where
  runMessage msg e@(WhiteQueenMagneto attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- filterM (\character -> anyM (\status -> selectAny (CharacterWithId character <> CharacterWithStatus status)) [StunnedStatus, ConfusedStatus, ToughStatus]) =<< selectList AnyCharacter
      chooseOne you [targeting (characterRef character) $ cardSpecific attrs "clearStatus" character | character <- characters]
      pure e
    CardSpecific (isTarget attrs -> True) "clearStatus" (fromJSON -> Success character) -> do
      statuses <- filterM (\s -> selectAny (CharacterWithId character <> CharacterWithStatus s)) [StunnedStatus, ConfusedStatus, ToughStatus]
      chooseOne (allyController attrs) [labeled (case s of StunnedStatus -> "$label.stunned"; ConfusedStatus -> "$label.confused"; ToughStatus -> "$label.tough") $ removeStatus character s | s <- statuses]
      pure e
    _ -> WhiteQueenMagneto <$> runMessage msg attrs
