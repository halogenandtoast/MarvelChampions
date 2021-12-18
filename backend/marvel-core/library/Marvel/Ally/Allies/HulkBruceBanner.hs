module Marvel.Ally.Allies.HulkBruceBanner (
  hulkBruceBanner,
  HulkBruceBanner (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs hiding (AllyAttacked)
import Marvel.Ally.Cards qualified as Cards
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Resource
import Marvel.Window

hulkBruceBanner :: AllyCard HulkBruceBanner
hulkBruceBanner =
  ally HulkBruceBanner Cards.hulkBruceBanner (Thw 0, 0) (Atk 3, 1) (HP 5)

newtype HulkBruceBanner = HulkBruceBanner AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget, HasController)

instance HasAbilities HulkBruceBanner where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (AllyAttacked After (AllyWithId $ toId a) AnyEnemy)
        ForcedResponse
        OwnsThis
        NoCost
        $ runAbility a 1
    ]

instance RunMessage HulkBruceBanner where
  runMessage msg a = case msg of
    RanAbility (isTarget a -> True) 1 _ -> do
      push . controllerMessage a $ DiscardFrom FromDeck 1 (Just $ toTarget a)
      pure a
    WithDiscarded (isTarget a -> True) _ (onlyPlayerCards -> cards) -> do
      case cards of
        [] -> pure ()
        _ -> do
          let resources =
                hashNub (concatMap (printedResources . getCardDef) cards)
              toChoice = \case
                Physical ->
                  [ Label
                      "Deal 2 damage to an enemy"
                      [ ChooseDamage
                          (toSource a)
                          (toDamage 2 FromAbility)
                          AnyEnemy
                      ]
                  ]
                Energy ->
                  [ Label
                      "Deal 1 damage to each character"
                      [ DamageAllCharacters
                          AnyCharacter
                          (toSource a)
                          (toDamage 1 FromAbility)
                      ]
                  ]
                Mental -> [Label "Discard Hulk" [DiscardTarget $ toTarget a]]
                Wild -> toChoice Physical <> toChoice Energy <> toChoice Mental
              choices = concatMap toChoice resources
          chooseOneAtATime (controller a) choices
      pure a
    _ -> HulkBruceBanner <$> runMessage msg (toAttrs a)
