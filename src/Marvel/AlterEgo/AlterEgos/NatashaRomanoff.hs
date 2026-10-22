module Marvel.AlterEgo.AlterEgos.NatashaRomanoff where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait
import Marvel.Window qualified as W

natashaRomanoff :: AlterEgoCard NatashaRomanoff
natashaRomanoff =
  alterEgo
    NatashaRomanoff
    Cards.natashaRomanoff
    (HP $ Static 9)
    (HandSize 6)
    (Rec 3)
    [Cards.burnNotice]

newtype NatashaRomanoff = NatashaRomanoff (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Mission Prep: after you play a Preparation card, draw 1 card (limit once per phase).
instance HasAbilities NatashaRomanoff where
  getAbilities a@(NatashaRomanoff attrs) =
    [ label "Mission Prep" $
        limited (PerPhase 1) $
          limitedWindowAbility
            a
            1
            (W.PlaysCard W.After (IdentityWithId $ alterEgoIdentityId attrs) (CardWithTrait Preparation))
            Response
            IsSelf
            NoCost
            (Run [IdentityMessage (alterEgoIdentityId attrs) (DrawCards FromDeck 1)])
    ]

instance RunMessage NatashaRomanoff where
  runMessage msg (NatashaRomanoff attrs) = NatashaRomanoff <$> runMessage msg attrs
