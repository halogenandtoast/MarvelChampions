module Marvel.Ally.Allies.SpiderManMilesMorales (
  spiderManMilesMorales,
  spiderManMilesMoralesEffect,
  SpiderManMilesMorales (..),
  SpiderManMilesMoralesEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Def ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Window

spiderManMilesMorales :: AllyCard SpiderManMilesMorales
spiderManMilesMorales = ally SpiderManMilesMorales Cards.spiderManMilesMorales (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype SpiderManMilesMorales = SpiderManMilesMorales (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: After you play Spider-Man from your hand, choose THW or ATK. Spider-Man
-- gets +2 to the chosen power until the end of the phase.
instance HasAbilities SpiderManMilesMorales where
  getAbilities a@(SpiderManMilesMorales attrs) =
    [ limitedWindowAbility a 1 (PlaysCard After (IdentityWithId $ allyController attrs) (CardIs Cards.spiderManMilesMorales)) Response OwnsThis NoCost $
        runAbility a 1
    ]

instance RunMessage SpiderManMilesMorales where
  runMessage msg a@(SpiderManMilesMorales attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      createEffect Cards.spiderManMilesMorales a (AllyEntity $ AllyWithId $ allyId attrs)
      pure a
    _ -> SpiderManMilesMorales <$> runMessage msg attrs

newtype SpiderManMilesMoralesEffect = SpiderManMilesMoralesEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

spiderManMilesMoralesEffect :: CardEffect SpiderManMilesMoralesEffect
spiderManMilesMoralesEffect =
  effectWith SpiderManMilesMoralesEffect Cards.spiderManMilesMorales (endsL ?~ DisableAtEndOfPhase)

instance RunMessage SpiderManMilesMoralesEffect where
  runMessage msg e@(SpiderManMilesMoralesEffect attrs) = runQueueT $ case msg of
    EffectMessage ident msg' | ident == effectId attrs -> case msg' of
      UsedEffect you -> do
        chooseOne you
          [ labeled "$label.thwart" $ effectChoice ident 1
          , labeled "$label.attack" $ effectChoice ident 2
          ]
        pure e
      EffectChoice 1 -> pure . SpiderManMilesMoralesEffect $ attrs & modifiersL .~ [ThwartModifier 2]
      EffectChoice 2 -> pure . SpiderManMilesMoralesEffect $ attrs & modifiersL .~ [AttackModifier 2]
      _ -> SpiderManMilesMoralesEffect <$> runMessage msg attrs
    _ -> SpiderManMilesMoralesEffect <$> runMessage msg attrs
