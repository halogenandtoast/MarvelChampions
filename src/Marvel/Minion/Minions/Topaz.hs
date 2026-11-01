module Marvel.Minion.Minions.Topaz where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Domino ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Matchers ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

topaz :: MinionCard Topaz
topaz = minion Topaz Cards.topaz (Sch 1) (Atk 2) (HP 3)
newtype Topaz = Topaz (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Topaz where
  runMessage msg a@(Topaz attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      searchEverywhereAndPutIntoPlayEncounterCard you (CardWithTitle "Superpower Feedback")
      pure a
    _ -> Topaz <$> runMessage msg attrs
