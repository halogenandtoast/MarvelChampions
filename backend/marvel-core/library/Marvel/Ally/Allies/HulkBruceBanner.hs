module Marvel.Ally.Allies.HulkBruceBanner (
  hulkBruceBanner,
  HulkBruceBanner (..),
) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message hiding (AllyAttacked)
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

hulkBruceBanner :: AllyCard HulkBruceBanner
hulkBruceBanner =
  ally HulkBruceBanner Cards.hulkBruceBanner (Thw 0, 0) (Atk 3, 1) (HP 5)

newtype HulkBruceBanner = HulkBruceBanner AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities HulkBruceBanner where
  getAbilities (HulkBruceBanner a) =
    [ limitedWindowAbility
        a
        1
        (AllyAttacked After (AllyWithId $ toId a) AnyEnemy)
        ForcedResponse
        OwnsThis
        NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage HulkBruceBanner where
  runMessage msg a@(HulkBruceBanner attrs) = case msg of
    RanAbility target 1 _
      | isTarget attrs target ->
        a
          <$ push
            ( IdentityMessage (allyController attrs) $
                DiscardFrom FromDeck 1 (Just $ toTarget attrs)
            )
    WithDiscarded target _ (onlyPlayerCards -> cards) | isTarget attrs target ->
      do
        case cards of
          [] -> pure ()
          _ -> do
            let resources =
                  HashSet.toList $
                    HashSet.fromList
                      (concatMap (printedResources . getCardDef) cards)
                toChoice = \case
                  Physical ->
                    [ Label
                        "Deal 2 damage to an enemy"
                        [ ChooseDamage
                            (toSource attrs)
                            (toDamage 2 FromAbility)
                            AnyEnemy
                        ]
                    ]
                  Energy ->
                    [ Label
                        "Deal 1 damage to each character"
                        [ DamageAllCharacters
                            AnyCharacter
                            (toSource attrs)
                            (toDamage 1 FromAbility)
                        ]
                    ]
                  Mental -> [Label "Discard Hulk" [DiscardTarget target]]
                  Wild -> toChoice Physical <> toChoice Energy <> toChoice Mental
                choices = concatMap toChoice resources
            chooseOneAtATime (allyController attrs) choices
        pure a
    _ -> HulkBruceBanner <$> runMessage msg attrs
