module Marvel.Ally.Allies.HulkBruceBanner (
  hulkBruceBanner,
  HulkBruceBanner (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner hiding (AllyAttacked)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Resource
import Marvel.Window

hulkBruceBanner :: AllyCard HulkBruceBanner
hulkBruceBanner =
  ally HulkBruceBanner Cards.hulkBruceBanner (Thw 0, 0) (Atk 3, 1) (HP 5)

newtype HulkBruceBanner = HulkBruceBanner (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef, HasController)

instance HasAbilities HulkBruceBanner where
  getAbilities (HulkBruceBanner a) =
    [ limitedWindowAbility
        a
        1
        (AllyAttacked After (AllyWithId $ allyId a) AnyEnemy)
        ForcedResponse
        OwnsThis
        NoCost
        $ runAbility a 1
    ]

instance RunMessage HulkBruceBanner where
  runMessage msg a@(HulkBruceBanner attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      discardFromDeck ident 1 a
      pure a
    WithDiscarded (isTarget a -> True) _ (onlyPlayerCards -> cards) -> do
      let
        resources =
          hashNub (concatMap (printedResources . getCardDef) cards)
        -- each its own step: one chosen sees the game after the one before it
        toChoice = \case
          Physical -> [labeled "$label.deal2DamageToAnEnemy" $ cardSpecific a "resource" Physical]
          Energy -> [labeled "$label.deal1DamageToEachCharacter" $ cardSpecific a "resource" Energy]
          Mental -> [labeled "$label.discardHulk" $ cardSpecific a "resource" Mental]
          Wild -> toChoice Physical <> toChoice Energy <> toChoice Mental
      unless (null cards) $ chooseOneAtATime (controller a) (concatMap toChoice resources)
      pure a
    CardSpecific (isTarget a -> True) "resource" (fromJSON -> Success resource) -> do
      case resource of
        Physical -> chooseDamage (controller a) a (toDamage 2 FromAbility) AnyEnemy
        Energy -> damageAllCharacters a (toDamage 1 FromAbility) AnyCharacter
        Mental -> removeFromPlay a
        Wild -> pure ()
      pure a
    _ -> HulkBruceBanner <$> runMessage msg attrs
