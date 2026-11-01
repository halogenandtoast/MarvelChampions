module Marvel.Upgrade.Upgrades.PsiKnife (psiKnife, PsiKnife (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Query ()
import Marvel.Question
import Marvel.Payment ()
import Marvel.Resource
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Keyword qualified as Keyword
import Marvel.Card
import Marvel.Game.Source
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

psiKnife :: UpgradeCard PsiKnife
psiKnife = upgrade PsiKnife Cards.psiKnife

newtype PsiKnife = PsiKnife (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor PsiKnife where
  getModifiersFor _ (IdentityRef you) (PsiKnife a) | you == upgradeController a = do
    psylocke <- identityMatches (HeroIdentity <> IdentityWithTitle "Psylocke") you
    pure $ if not psylocke then [] else if toCardCode a == "41002a"
      then [ThwartModifier 1] else [AttackModifier 1, BasicAttackKeyword Keyword.Piercing]
  getModifiersFor _ _ _ = pure []

instance HasAbilities PsiKnife where
  getAbilities (PsiKnife a) =
    [(ability a 1 HeroResource OwnsThis ExhaustCost $ Pay $ ResourcePayment resource)
      {abilityChoices = [Pay (ResourcePayment resource), runAbility a 1]}]
    where resource = if toCardCode a == "41002a" then Mental else Physical

instance RunMessage PsiKnife where
  runMessage msg e@(PsiKnife a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      modifiers <- getModifiers a
      unless (CannotFlipPsiEnergy `elem` modifiers) $ chooseOne you
        [ labeled "$label.flipPsiEnergy" (flipPsiEnergy $ upgradeId a)
        , labeled "$label.done" (pure ())
        ]
      pure e
    CardSpecific (isTarget a -> True) "flipPsiEnergy" _ -> do
      modifiers <- getModifiers a
      if CannotFlipPsiEnergy `elem` modifiers then pure e else do
        identityStep (upgradeController a) CheckRestrictedLimit
        pure $ PsiKnife a {upgradeCardDef = if toCardCode a == "41002a" then Cards.psiKatana else Cards.psiKnife}
    _ -> PsiKnife <$> runMessage msg a
