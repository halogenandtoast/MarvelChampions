module Marvel.Minion.Minions.MUSICShuri where
import Marvel.Prelude
import Marvel.Count
import Marvel.Query
import Marvel.Id
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Minion.Runner

mUSICShuri :: MinionCard MUSICShuri
mUSICShuri = minion MUSICShuri Cards.mUSICShuri (Sch 0) (Atk 0) (HP 1)
newtype MUSICShuri = MUSICShuri (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage MUSICShuri where
  runMessage msg u@(MUSICShuri attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Cards.manipulatedMUSICShuri)
      pure u
    MinionMessage mid MinionDefeated | mid == minionId attrs -> do
      schemes <- selectList (SideSchemeIs Cards.manipulatedMUSICShuri)
      mainSchemes <- selectList EncounterMainScheme
      for_ schemes $ \sid -> do
        n <- selectCount SchemeThreat (SchemeWithId $ SchemeSideSchemeId sid)
        for_ (headMay mainSchemes) $ \main -> moveSchemeThreat attrs (SchemeSideSchemeId sid) main n
      pure u
    _ -> MUSICShuri <$> runMessage msg attrs
