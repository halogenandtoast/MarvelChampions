module Marvel.Upgrade.Upgrades.TechnoOrganicWings where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Matchers qualified as Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

technoOrganicWings :: UpgradeCard TechnoOrganicWings
technoOrganicWings = upgrade TechnoOrganicWings Cards.technoOrganicWings

newtype TechnoOrganicWings = TechnoOrganicWings (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TechnoOrganicWings where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis <> AnyCriteria
      [SelfMatches (IdentityWithTitle "Angel" <> Matchers.ExhaustedIdentity), SelfMatches (IdentityWithTitle "Archangel")])
      ExhaustCost (runAbility a 1)]

instance RunMessage TechnoOrganicWings where
  runMessage msg a@(TechnoOrganicWings attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      angel <- identityMatches (IdentityWithTitle "Angel") you
      archangel <- identityMatches (IdentityWithTitle "Archangel") you
      when angel $ ready you
      when archangel $ createEffect Cards.technoOrganicWings attrs (IdentityEntity $ IdentityWithId you)
      pure a
    _ -> TechnoOrganicWings <$> runMessage msg attrs

aerialEvent :: CardMatcher
aerialEvent = CardWithType EventType <> CardWithTrait Trait.Aerial

-- Remember which physical card was initiated from hand so plays from another zone
-- cannot spend the pending discount.
newtype WingsMeta = WingsMeta {wingPlay :: Maybe CardId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TechnoOrganicWingsEffect = TechnoOrganicWingsEffect (Attrs Effect `With` WingsMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsEffect TechnoOrganicWingsEffect where
  toEffectAttrs (TechnoOrganicWingsEffect (attrs `With` _)) = attrs

instance IsRef TechnoOrganicWingsEffect where
  toRef = toRef . toEffectAttrs

technoOrganicWingsEffect :: CardEffect TechnoOrganicWingsEffect
technoOrganicWingsEffect = effectWith (TechnoOrganicWingsEffect . (`With` WingsMeta Nothing)) Cards.technoOrganicWings $
  (modifiersL .~ [ResourceCostReductionForHandCard aerialEvent 2]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage TechnoOrganicWingsEffect where
  runMessage msg e@(TechnoOrganicWingsEffect (attrs `With` meta)) = runQueueT $ case msg of
    IdentityMessage ident (PlayingCard card _) | cardMatch aerialEvent card -> do
      applies <- effectValidFor attrs (toRef ident)
      held <- elem card <$> projectMap PlayerIdentityHand unHand ident
      pure $ if applies && held
        then TechnoOrganicWingsEffect (attrs `With` WingsMeta (Just $ pcCardId card))
        else e
    IdentityMessage ident (PlayedCard card _) | wingPlay meta == Just (pcCardId card) -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    _ -> TechnoOrganicWingsEffect . (`With` meta) <$> runMessage msg attrs
