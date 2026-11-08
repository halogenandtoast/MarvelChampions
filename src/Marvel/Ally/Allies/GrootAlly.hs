module Marvel.Ally.Allies.GrootAlly (
  grootAlly,
  GrootAlly (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Matchers
import Marvel.Window

grootAlly :: AllyCard GrootAlly
grootAlly = ally GrootAlly Cards.grootAlly (Thw 1, 1) (Atk 2, 2) (HP 6)

newtype GrootAlly = GrootAlly (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Response: After Groot defends against an attack, heal 2 damage from him." A Groot
-- the attack defeated is no longer in play to answer it.
instance HasAbilities GrootAlly where
  getAbilities a@(GrootAlly attrs) =
    [ limitedWindowAbility
        a
        1
        (AllyDefends $ AllyWithId $ allyId attrs)
        Response
        (OwnsThis <> CharacterExists (AllyCharacters (AllyWithId $ allyId attrs) <> healableCharacter))
        NoCost
        $ Heal (AllyCharacter $ allyId attrs) 2
    ]

instance RunMessage GrootAlly where
  runMessage msg (GrootAlly attrs) = GrootAlly <$> runMessage msg attrs
