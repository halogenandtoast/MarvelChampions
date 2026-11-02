module Marvel.SideScheme.SideSchemes.ManipulatedMUSICShuri where
import Marvel.Prelude
import Marvel.Id ()
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

manipulatedMUSICShuri :: SideSchemeCard ManipulatedMUSICShuri
manipulatedMUSICShuri = sideScheme ManipulatedMUSICShuri Cards.manipulatedMUSICShuri (Static 5)
newtype ManipulatedMUSICShuri = ManipulatedMUSICShuri (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ManipulatedMUSICShuri where
  runMessage msg u@(ManipulatedMUSICShuri attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      you <- getActivePlayerId
      searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.mUSICShuri)
      pure u
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      minions <- selectList (MinionIs Cards.mUSICShuri)
      traverse_ removeFromPlay minions
      pure u
    _ -> ManipulatedMUSICShuri <$> runMessage msg attrs
