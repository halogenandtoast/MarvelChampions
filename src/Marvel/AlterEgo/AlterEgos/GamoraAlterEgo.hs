module Marvel.AlterEgo.AlterEgos.GamoraAlterEgo (
  gamoraAlterEgo,
  GamoraAlterEgo (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Deck
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait qualified as Trait

gamoraAlterEgo :: AlterEgoCard GamoraAlterEgo
gamoraAlterEgo =
  alterEgo
    GamoraAlterEgo
    Cards.gamoraAlterEgo
    (HP $ Static 10)
    (HandSize 6)
    (Rec 3)
    [Cards.unfulfilledDestiny]

newtype GamoraAlterEgo = GamoraAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Skilled Tactician is a deck-building rule; the engine checks no deck's aspects, so
-- it needs nothing here. Action: look at the top card of your deck; if it's an Attack or
-- Thwart event, draw it. (Limit once per round.)
instance HasAbilities GamoraAlterEgo where
  getAbilities a =
    [limitedAbility a 1 (PerRound 1) Action IsSelf NoCost $ runAbility a 1]

instance RunMessage GamoraAlterEgo where
  runMessage msg ae@(GamoraAlterEgo a) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      top <- take 1 <$> projectMap PlayerIdentityDeck unDeck ident
      for_ top \c ->
        lookingAt ident [PlayerCard c] $
          chooseOne ident $
            if cardMatch (CardWithType EventType <> CardMatchesAny [CardWithTrait Trait.Attack, CardWithTrait Trait.Thwart]) c
              then [labeled (countLabel "drawCards" 1) (drawCards ident 1)]
              else [labeled "$label.done" (pure ())]
      pure ae
    _ -> GamoraAlterEgo <$> runMessage msg a
