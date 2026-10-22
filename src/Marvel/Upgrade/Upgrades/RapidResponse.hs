module Marvel.Upgrade.Upgrades.RapidResponse (
  rapidResponse,
  RapidResponse (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

rapidResponse :: UpgradeCard RapidResponse
rapidResponse = upgrade RapidResponse Cards.rapidResponse

newtype RapidResponse = RapidResponse (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RapidResponse where
  getAbilities a@(RapidResponse attrs) =
    [ limitedWindowAbility
        a
        1
        (AllyWasDefeated After (IdentityWithId $ upgradeController attrs))
        HeroResponse
        OwnsThis
        (DiscardCost $ toTarget a)
        (runAbility a 1)
    ]

-- | The ally comes back from the discard pile, if it is still there, and takes 1 damage.
instance RunMessage RapidResponse where
  runMessage msg u@(RapidResponse attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 [AllyCardDefeated _ card] _ -> do
      discard <- projectMap PlayerIdentityDiscard unDiscard ident
      when (card `elem` discard) $
        pushAll
          [ RemoveFromGame (toTarget card)
          , PutCardIntoPlay ident card NoPayment Nothing
          , AllyMessage (AllyId $ unCardId $ pcCardId card) $
              AllyDamaged (toSource attrs) (toDamage 1 FromAbility)
          ]
      pure u
    _ -> RapidResponse <$> runMessage msg attrs
