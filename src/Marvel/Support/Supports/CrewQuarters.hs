module Marvel.Support.Supports.CrewQuarters (
  crewQuarters,
  CrewQuarters (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

crewQuarters :: SupportCard CrewQuarters
crewQuarters = support CrewQuarters Cards.crewQuarters

newtype CrewQuarters = CrewQuarters (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Play under any player's control. Max 1 per player (the card def). Alter-Ego Action:
-- Exhaust Crew Quarters → heal 1 damage from an alter-ego, any player's. Only a damaged
-- alter-ego is a valid target, and with none the ability can't be used (RR Target).
instance HasAbilities CrewQuarters where
  getAbilities a =
    [ ability a 1 AlterEgoAction (OwnsThis <> CharacterExists (IdentityCharacters damagedAlterEgo)) ExhaustCost $
        runAbility a 1
    ]

damagedAlterEgo :: IdentityMatcher
damagedAlterEgo = AlterEgoIdentity <> IdentityWithDamage (GreaterThan (Static 0))

instance RunMessage CrewQuarters where
  runMessage msg s@(CrewQuarters attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      alterEgos <- selectList damagedAlterEgo
      chooseOne you [targeting alterEgo (heal alterEgo 1) | alterEgo <- alterEgos]
      pure s
    _ -> CrewQuarters <$> runMessage msg attrs
