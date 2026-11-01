module Marvel.Obligation.Obligations.MemoriesofArmageddon where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Card
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Domino
import Marvel.Deck
import Marvel.Discard
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

memoriesofArmageddon :: ObligationCard MemoriesofArmageddon
memoriesofArmageddon = obligation MemoriesofArmageddon Cards.memoriesofArmageddon
newtype MemoriesofArmageddon = MemoriesofArmageddon (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MemoriesofArmageddon where
  getAbilities a@(MemoriesofArmageddon attrs) =
    [ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId you) ExhaustCost (runAbility a 1) | you <- maybeToList $ obligationOwner attrs]

instance HasModifiersFor MemoriesofArmageddon where
  getBlankingModifiersFor _ (IdentityRef you) (MemoriesofArmageddon attrs) = pure [Blank | obligationOwner attrs == Just you]
  getBlankingModifiersFor _ _ _ = pure []

instance RunMessage MemoriesofArmageddon where
  runMessage msg a@(MemoriesofArmageddon attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation revealedFor) | oid == obligationId attrs -> do
      owner <- selectOne (IdentityIs Heroes.dominoHero)
      let you = fromMaybe revealedFor owner
      pure $ MemoriesofArmageddon attrs {obligationOwner = Just you}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> MemoriesofArmageddon <$> runMessage msg attrs
