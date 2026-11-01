module Marvel.Obligation.Obligations.BodySwapped where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

bodySwapped :: ObligationCard BodySwapped
bodySwapped = obligation BodySwapped Cards.bodySwapped

newtype BodySwapped = BodySwapped (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor BodySwapped where
  getModifiersFor _ (UpgradeRef uid) (BodySwapped attrs) = case obligationOwner attrs of
    Just you -> do
      locked <- selectAny $ UpgradeWithId uid <> UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKatana
      pure [CannotFlipPsiEnergy | locked]
    Nothing -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities BodySwapped where
  getAbilities a@(BodySwapped attrs) =
    [ ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId you)
        (DiscardMatchingHandCardCost $ CardWithTrait Trait.Psionic) (runAbility a 1)
    | Just you <- [obligationOwner attrs] ]

instance RunMessage BodySwapped where
  runMessage msg a@(BodySwapped attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation revealedFor) | oid == obligationId attrs -> do
      owner <- selectOne $ IdentityIs Heroes.psylockeHero
      let you = fromMaybe revealedFor owner
      knives <- selectList $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKnife
      for_ knives $ \uid -> cardSpecific uid "flipPsiEnergy" ()
      weapons <- selectList $ UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait Trait.PsiEnergy
      traverse_ exhaust weapons
      pure $ BodySwapped attrs {obligationOwner = Just you}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> BodySwapped <$> runMessage msg attrs
