module Marvel.Upgrade.Upgrades.HonedTechnique where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Aspect
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

honedTechnique :: UpgradeCard HonedTechnique
honedTechnique = upgrade HonedTechnique Cards.honedTechnique

newtype HonedTechnique = HonedTechnique (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HonedTechnique where
  getAbilities a@(HonedTechnique attrs) =
    [ limitedWindowAbility a 1
        (W.PlaysEventPaidWith W.When (IdentityWithId $ upgradeController attrs)
          (CardWithAspect Aggression <> CardWithTrait Trait.Attack) Mental)
        Interrupt OwnsThis NoCost (runAbility a 1)
    ]

instance RunMessage HonedTechnique where
  runMessage msg a@(HonedTechnique attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [eid | W.EventPlayed _ eid _ <- windows] $ createEffect Cards.honedTechnique attrs . EventEntity
      pure a
    _ -> HonedTechnique <$> runMessage msg attrs

-- Every damage instance from the resolving event gets its printed-cost bonus.
newtype HonedTechniqueEffect = HonedTechniqueEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

honedTechniqueEffect :: CardEffect HonedTechniqueEffect
honedTechniqueEffect = effect HonedTechniqueEffect Cards.honedTechnique

instance RunMessage HonedTechniqueEffect where
  runMessage msg (HonedTechniqueEffect attrs) = runQueueT $ case msg of
    EffectMessage ident (UsedEffect _) | ident == effectId attrs, EventEntity eid <- effectMatcher attrs -> do
      def <- getsGame (\g -> abilityCardDef g (EventRef eid))
      pure $ HonedTechniqueEffect $ attrs & modifiersL .~ [DamageDealtModifier $ fromIntegral $ max 0 $ fromMaybe 0 $ def >>= cdCost]
    _ -> HonedTechniqueEffect <$> runMessage msg attrs
