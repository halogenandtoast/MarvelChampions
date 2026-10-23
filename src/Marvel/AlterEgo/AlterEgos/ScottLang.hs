module Marvel.AlterEgo.AlterEgos.ScottLang where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

scottLang :: AlterEgoCard ScottLang
scottLang =
  alterEgo
    ScottLang
    Cards.scottLang
    (HP $ Static 12)
    (HandSize 6)
    (Rec 3)
    [Cards.careForCassie]

newtype ScottLang = ScottLang (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Time to Unwind — Response: After you change to this form, heal 1 damage from Scott
-- Lang.
instance HasAbilities ScottLang where
  getAbilities a@(ScottLang attrs) =
    [ label "Time to Unwind" $
        limitedWindowAbility a 1 (W.IdentityChangedToForm W.After $ IdentityWithId $ alterEgoIdentityId attrs) Response IsSelf NoCost $
          Heal (IdentityCharacter $ alterEgoIdentityId attrs) 1
    ]

instance RunMessage ScottLang where
  runMessage msg (ScottLang a) = ScottLang <$> runMessage msg a
