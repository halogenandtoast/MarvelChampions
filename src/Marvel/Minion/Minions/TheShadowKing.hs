module Marvel.Minion.Minions.TheShadowKing where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Stats ()
import Marvel.Trait qualified as Trait
import Marvel.Attachment.Cards qualified as Attachments

theShadowKing :: MinionCard TheShadowKing
theShadowKing = minion TheShadowKing Cards.theShadowKing (Sch 3) (Atk 3) (HP 6)

newtype TheShadowKing = TheShadowKing (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor TheShadowKing where
  getModifiersFor _ (MinionRef mid) (TheShadowKing attrs) | mid == minionId attrs = do
    controlled <- selectAny (MinionWithTrait Trait.Controlled)
    pure [CannotTakeDamage | controlled]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheShadowKing where
  runMessage msg (TheShadowKing attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      searchAndRevealEncounterCard you (CardIs Attachments.possessed)
      TheShadowKing <$> runMessage msg attrs
    _ -> TheShadowKing <$> runMessage msg attrs
