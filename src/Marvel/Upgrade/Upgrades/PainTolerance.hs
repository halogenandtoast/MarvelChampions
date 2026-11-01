module Marvel.Upgrade.Upgrades.PainTolerance where

import Marvel.Prelude
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

painTolerance :: UpgradeCard PainTolerance
painTolerance = upgrade PainTolerance Cards.painTolerance

newtype PainTolerance = PainTolerance (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

x23Cards :: CardMatcher
x23Cards = CardMatchesAny $ map CardWithTitle
  ["X-23's Claws", "Honey Badger", "Animal Instinct", "Claw Mastery", "Regenerative Longevity", "Sisterly Bond", "Sisterhood", "Adamantium Lacing", "Grim Resolve", "Pain Tolerance", "Puncture Wound"]

instance HasAbilities PainTolerance where
  getAbilities a@(PainTolerance attrs) =
    [ limitedWindowAbility a 1 (W.PlaysCard W.After (IdentityWithId $ upgradeController attrs) x23Cards)
        Response (OwnsThis <> CharacterExists (IdentityCharacters (IdentityWithId $ upgradeController attrs) <> CharacterCanBeHealed <> CharacterWithAnyDamage)) NoCost (runAbility a 1)
    ]

instance RunMessage PainTolerance where
  runMessage msg a@(PainTolerance attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> a <$ heal you 1
    _ -> PainTolerance <$> runMessage msg attrs
