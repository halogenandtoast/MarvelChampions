module Marvel.Upgrade.Upgrades.Attacrobatics (
  attacrobatics,
  Attacrobatics (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Boost
import Marvel.Card
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

attacrobatics :: UpgradeCard Attacrobatics
attacrobatics = upgrade Attacrobatics Cards.attacrobatics

newtype Attacrobatics = Attacrobatics (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Only a boost card with boost icons, as there are none to cancel otherwise.
instance HasAbilities Attacrobatics where
  getAbilities a =
    [ subtype Ability.Attack $
        limitedWindowAbility
          a
          1
          (BoostCardTurnedFaceup When EncounterCardWithBoostIcons)
          HeroInterrupt
          OwnsThis
          (DiscardCost $ toTarget a)
          (runAbility a 1)
    ]

instance RunMessage Attacrobatics where
  runMessage msg u@(Attacrobatics attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 [FlippedBoostCard card _] _ ->
      attackGuard ident u do
        -- the card's icons are added next: they are not
        cancelMatchingMessage \case
          VillainMessage _ (VillainAddBoost _) -> True
          _ -> False
        let icons = boostCount (cdBoostIcons $ getCardDef card)
        -- an attack on the villain, which guard forbids
        guarded <- selectAny (MinionWithKeyword Guard <> MinionEngagedWith (IdentityWithId ident))
        villain <- selectJust ActiveVillain
        unless guarded $
          pushChoice ident $
            DamageEnemy (toRef villain) (toSource attrs) (toDamage icons $ FromPlayerAttack ident)
        pure u
    _ -> Attacrobatics <$> runMessage msg attrs
