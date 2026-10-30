module Marvel.SideScheme.SideSchemes.TheHellfireClub where

import Marvel.Prelude
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Minion.Cards qualified as Enemies

theHellfireClub :: SideSchemeCard TheHellfireClub
theHellfireClub = sideScheme TheHellfireClub Cards.theHellfireClub (PerPlayer 2)

newtype TheHellfireClub = TheHellfireClub (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheHellfireClub where
  runMessage msg e@(TheHellfireClub attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      searchEverywhereAndPutIntoPlayEncounterCard you (CardIs Enemies.hellfirePawn)
      pure e
    _ -> TheHellfireClub <$> runMessage msg attrs
