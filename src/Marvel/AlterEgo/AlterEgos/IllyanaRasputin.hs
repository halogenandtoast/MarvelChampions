module Marvel.AlterEgo.AlterEgos.IllyanaRasputin where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

illyanaRasputin :: AlterEgoCard IllyanaRasputin
illyanaRasputin = alterEgo IllyanaRasputin Cards.illyanaRasputin (HP $ Static 10) (HandSize 6) (Rec 3) [Obligations.darkchilde]

newtype IllyanaRasputin = IllyanaRasputin (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IllyanaRasputin where
  getAbilities (IllyanaRasputin attrs) =
    [ limited (PerPhase 1) $ limitedWindowAbility attrs 1
        (W.IdentityChangedToForm W.When $ IdentityWithId $ alterEgoIdentityId attrs)
        Interrupt (IsSelf <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardWithTrait Trait.Spell))
        NoCost (runAbility attrs 1)
    ]

instance RunMessage IllyanaRasputin where
  runMessage msg a@(IllyanaRasputin attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithTrait Trait.Spell)
      chooseOne you [targeting card (placeOnTopOfDeck you card) | card <- cards]
      pure a
    _ -> IllyanaRasputin <$> runMessage msg attrs
