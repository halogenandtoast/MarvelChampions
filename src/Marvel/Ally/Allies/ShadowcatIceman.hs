module Marvel.Ally.Allies.ShadowcatIceman where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Stats
import Marvel.Entity
import Marvel.Effect.Types
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Ally.Runner

shadowcatIceman :: AllyCard ShadowcatIceman
shadowcatIceman = ally ShadowcatIceman Cards.shadowcatIceman (Thw 2, 1) (Atk 1, 1) (HP 3)

newtype ShadowcatIceman = ShadowcatIceman (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ShadowcatIceman where
  getAbilities a@(ShadowcatIceman attrs) = [limitedWindowAbility a 1
    (W.PlayedFromHand (IdentityWithId $ allyController attrs) $ CardIs Cards.shadowcatIceman) Response
    (OwnsThis <> SideSchemeExists AnySideScheme) NoCost (runAbility a 1)]
instance RunMessage ShadowcatIceman where
  runMessage msg a@(ShadowcatIceman attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      schemes <- selectList AnySideScheme
      chooseOne you [targeting sid $ createEffect Cards.shadowcatIceman attrs (SideSchemeEntity $ SideSchemeWithId sid) | sid <- schemes]
      pure a
    _ -> ShadowcatIceman <$> runMessage msg attrs

newtype ShadowcatIcemanEffect = ShadowcatIcemanEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
shadowcatIcemanEffect :: CardEffect ShadowcatIcemanEffect
shadowcatIcemanEffect = effectWith ShadowcatIcemanEffect Cards.shadowcatIceman ((endsL ?~ DisableAtEndOfRound) . (modifiersL .~ [SuppressSchemeIcons]))
instance RunMessage ShadowcatIcemanEffect where
  runMessage msg (ShadowcatIcemanEffect attrs) = runQueueT $ ShadowcatIcemanEffect <$> runMessage msg attrs
