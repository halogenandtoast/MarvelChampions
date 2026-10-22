module Marvel.Upgrade.Upgrades.Espionage (
  espionage,
  Espionage (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

espionage :: UpgradeCard Espionage
espionage = upgrade Espionage Cards.espionage

newtype Espionage = Espionage (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Any player's surge: the card does not say "you".
instance HasAbilities Espionage where
  getAbilities a@(Espionage attrs) =
    [ limitedWindowAbility
        a
        1
        SurgeWouldResolve
        Interrupt
        OwnsThis
        (DiscardCost $ toTarget a)
        (Run [IdentityMessage (upgradeController attrs) (DrawCards FromDeck 2)])
    ]

instance RunMessage Espionage where
  runMessage msg (Espionage attrs) = Espionage <$> runMessage msg attrs
