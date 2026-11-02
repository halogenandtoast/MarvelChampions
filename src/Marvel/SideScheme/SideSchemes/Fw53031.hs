{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.SideScheme.SideSchemes.Fw53031 where
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
import Marvel.SideScheme.Types

fw53031 :: SideSchemeCard Fw53031
fw53031 = sideScheme Fw53031 Cards.fw53031 (Static 6)

newtype Fw53031 = Fw53031 (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme Fw53031 where
  sideSchemeAbilities = getAbilities

instance HasAbilities Fw53031 where
  getAbilities a@(Fw53031 attrs) =
    [limitedWindowAbility a 1 (W.EncounterCardDiscardedFromDeck (CardWithType MinionType <> CardWithTrait T.SerpentSociety)) ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage Fw53031 where
  runMessage msg a@(Fw53031 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      first <- selectJust FirstPlayer
      for_ [card | W.PlayerDiscardedFromEncounterDeck _ card <- windows, cardMatch (CardWithType MinionType <> CardWithTrait T.SerpentSociety) card] $ dealFoundEncounterCard first
      pure a
    _ -> Fw53031 <$> runMessage msg attrs
