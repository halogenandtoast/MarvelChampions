module Marvel.SideScheme.SideSchemes.InTheNameOfVengeance where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Keyword
import Marvel.GameValue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

inTheNameOfVengeance :: SideSchemeCard InTheNameOfVengeance
inTheNameOfVengeance = sideScheme InTheNameOfVengeance Cards.inTheNameOfVengeance (Static 6)

newtype InTheNameOfVengeance = InTheNameOfVengeance (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor InTheNameOfVengeance where
  getModifiersFor _ target _ = pure $ case target of
    VillainRef _ -> [KeywordModifier $ Retaliate 1]
    MinionRef _ -> [KeywordModifier $ Retaliate 1]
    _ -> []

instance RunMessage InTheNameOfVengeance where
  runMessage msg (InTheNameOfVengeance attrs) = runQueueT $ InTheNameOfVengeance <$> runMessage msg attrs
