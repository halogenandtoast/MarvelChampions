module Marvel.Upgrade.Upgrades.OperativeSkill where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Question (runAbility)
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

operativeSkill :: UpgradeCard OperativeSkill
operativeSkill = upgradeWith OperativeSkill Cards.operativeSkill ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype OperativeSkill = OperativeSkill (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities OperativeSkill where
  getAbilities a@(OperativeSkill attrs) =
    [ limitedWindowAbility a 1
        (W.WindowOneOf [W.HeroThwarted W.When owner, W.IdentityThwarts W.When owner])
        Interrupt OwnsThis UseCost (runAbility a 1)
    ]
   where
    owner = IdentityWithId $ upgradeController attrs

instance RunMessage OperativeSkill where
  runMessage msg a@(OperativeSkill attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      when (W.MadeBasicThwart you `elem` windows) $ addToBasicPower you 1
      for_ [source | W.IdentityThwart owner source <- windows, owner == you] $ \case
        EventRef eid -> createEffect Cards.operativeSkill attrs (EventEntity eid)
        IdentityRef ident -> createEffect Cards.operativeSkill attrs (IdentityEntity $ IdentityWithId ident)
        UpgradeRef uid -> createEffect Cards.operativeSkill attrs (UpgradeEntity $ UpgradeWithId uid)
        _ -> pure ()
      pure a
    _ -> OperativeSkill <$> runMessage msg attrs

newtype OperativeSkillEffect = OperativeSkillEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

operativeSkillEffect :: CardEffect OperativeSkillEffect
operativeSkillEffect = effectWith OperativeSkillEffect Cards.operativeSkill (modifiersL .~ [ThreatRemovedModifier 1])

instance RunMessage OperativeSkillEffect where
  runMessage msg a@(OperativeSkillEffect attrs) = runQueueT $ case msg of
    EventMessage eid ResolvedEvent | effectMatcher attrs == EventEntity eid -> a <$ disableEffect (effectId attrs)
    AbilityResolved source | onSource source -> a <$ disableEffect (effectId attrs)
    _ -> OperativeSkillEffect <$> runMessage msg attrs
   where
    onSource = \case
      IdentityRef ident -> effectMatcher attrs == IdentityEntity (IdentityWithId ident)
      UpgradeRef uid -> effectMatcher attrs == UpgradeEntity (UpgradeWithId uid)
      EventRef eid -> effectMatcher attrs == EventEntity eid
      _ -> False
