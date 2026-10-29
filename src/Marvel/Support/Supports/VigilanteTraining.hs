module Marvel.Support.Supports.VigilanteTraining (vigilanteTraining, VigilanteTraining (..)) where

import Marvel.Prelude
import Marvel.Aspect (Aspect (Justice))
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

vigilanteTraining :: SupportCard VigilanteTraining
vigilanteTraining = supportWith VigilanteTraining Cards.vigilanteTraining $ (usesL .~ 2) . (discardIfNoUsesL .~ True)

newtype VigilanteTraining = VigilanteTraining (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

justiceEvent :: CardMatcher
justiceEvent = CardWithType EventType <> CardWithAspect Justice

instance HasAbilities VigilanteTraining where
  getAbilities a@(VigilanteTraining attrs) =
    [ability a 1 AlterEgoAction
      (OwnsThis <> ExtendedCardExists (InDiscardOf (IdentityWithId $ supportController attrs) (BasicCardMatches justiceEvent)))
      (ExhaustCost <> UseCost) (runAbility a 1)]

instance RunMessage VigilanteTraining where
  runMessage msg s@(VigilanteTraining attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch justiceEvent) <$> projectMap PlayerIdentityDiscard unDiscard ident
      unless (null cards) $ chooseOne ident [targeting card (shuffleIntoDeck ident [card]) | card <- cards]
      pure s
    _ -> VigilanteTraining <$> runMessage msg attrs
