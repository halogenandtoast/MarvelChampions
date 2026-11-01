module Marvel.Upgrade.Upgrades.SuperhumanStrength (
  superhumanStrength,
  SuperhumanStrength (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Status ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

superhumanStrength :: UpgradeCard SuperhumanStrength
superhumanStrength = upgrade SuperhumanStrength Cards.superhumanStrength

newtype SuperhumanStrength = SuperhumanStrength (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SuperhumanStrength where
  getModifiersFor _ (IdentityRef ident) (SuperhumanStrength attrs)
    | ident == upgradeController attrs = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SuperhumanStrength where
  getAbilities (SuperhumanStrength a) =
    [ limitedWindowAbility
        a
        1
        (IdentityAttacked After You AnyEnemy)
        ForcedResponse
        OwnsThis
        (DiscardCost $ toTarget a)
        (RunAbility (toRef a) 1)
    ]

getAttackedCharacter :: [WindowType] -> Maybe CharacterId
getAttackedCharacter [] = Nothing
getAttackedCharacter (IdentityAttack _ enemy : _) = Just $ case enemy of
  EnemyVillainId vid -> VillainCharacter vid
  EnemyMinionId mid -> MinionCharacter mid
getAttackedCharacter (FriendlyPlayerAttack _ character : _) = Just character
getAttackedCharacter (FriendlyCharacterAttack (IdentityCharacter _) target : _) = Just (IdentityCharacter target)
getAttackedCharacter (_ : windows) = getAttackedCharacter windows

instance RunMessage SuperhumanStrength where
  runMessage msg u@(SuperhumanStrength attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (getAttackedCharacter windows) $ \case
        VillainCharacter vid -> stun vid attrs
        MinionCharacter mid -> stun mid attrs
        character -> giveStatus character StunnedStatus
      pure u
    _ -> SuperhumanStrength <$> runMessage msg attrs
