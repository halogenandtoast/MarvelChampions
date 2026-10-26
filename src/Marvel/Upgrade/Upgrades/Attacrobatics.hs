module Marvel.Upgrade.Upgrades.Attacrobatics (
  attacrobatics,
  Attacrobatics (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Boost
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
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
          -- an attack on the villain, which guard forbids
          (OwnsThis <> EnemyExists (VillainEnemy <> AttackableBy (IdentityWithId $ upgradeController $ toUpgradeAttrs a)))
          (DiscardCost $ toTarget a)
          (runAbility a 1)
    ]

instance RunMessage Attacrobatics where
  runMessage msg u@(Attacrobatics attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 [FlippedBoostCard card enemy] _ ->
      attackGuard ident u do
        -- the card's icons are counted and added next: they are not, but counted for
        -- the damage
        cancelBoostIcons enemy
        countBoostIcons attrs ident [card]
        pure u
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success ident) (sum . map snd -> icons) -> do
      -- with no villain in play (as between one Kang and the next), nothing
      selectOne ActiveVillain >>= \case
        Nothing -> pure u
        Just villain -> do
          dealDamage villain attrs (toDamage icons $ FromPlayerAttack ident)
          pure u
    _ -> Attacrobatics <$> runMessage msg attrs
