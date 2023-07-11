module Marvel.Ally.Allies.HulkBruceBanner (
  hulkBruceBanner,
  HulkBruceBanner (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner hiding (AllyAttacked)
import Marvel.Choice
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
  runMessage msg a@(HulkBruceBanner attrs) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      push . IdentityMessage ident $ DiscardFrom FromDeck 1 (Just $ toRef a)
      pure a
    WithDiscarded (isTarget a -> True) _ (onlyPlayerCards -> cards) -> do
      case cards of
        [] -> pure ()
        _ -> do
          let
            resources =
              hashNub (concatMap (printedResources . getCardDef) cards)
            toChoice = \case
              Physical ->
                [ Label
                    "Deal 2 damage to an enemy"
                    [ ChooseDamage
                        (toRef a)
                        (toDamage 2 FromAbility)
                        AnyEnemy
                    ]
                ]
              Energy ->
                [ Label
                    "Deal 1 damage to each character"
                    [ DamageAllCharacters
                        AnyCharacter
                        (toRef a)
                        (toDamage 1 FromAbility)
                    ]
                ]
              Mental -> [Label "Discard Hulk" [DiscardTarget $ toRef a]]
              Wild -> toChoice Physical <> toChoice Energy <> toChoice Mental
            choices = concatMap toChoice resources
          chooseOneAtATime (controller a) choices
      pure a
    _ -> HulkBruceBanner <$> runMessage msg attrs
