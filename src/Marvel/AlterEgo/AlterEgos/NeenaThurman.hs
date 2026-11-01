module Marvel.AlterEgo.AlterEgos.NeenaThurman where
import Marvel.Prelude
import Marvel.Card
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
import Marvel.Hand
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Obligation.Cards qualified as Obligations

neenaThurman :: AlterEgoCard NeenaThurman
neenaThurman = alterEgo NeenaThurman Cards.neenaThurman (HP $ Static 9) (HandSize 6) (Rec 3) [Obligations.memoriesofArmageddon]
newtype NeenaThurman = NeenaThurman (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities NeenaThurman where
  getAbilities a = [limitedAbility a 2 (PerRound 1) Action
    (IsSelf <> ExtendedCardExists (InHandOf You (BasicCardMatches AnyCard)) <> ExtendedCardExists (InDiscardOf You (BasicCardMatches AnyCard)))
    NoCost (runAbility a 2)]

instance RunMessage NeenaThurman where
  runMessage msg a@(NeenaThurman attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      hand <- projectMap PlayerIdentityHand unHand you
      chooseOne you [targeting card (cardSpecific you "swapHandWithDiscardTop" card) | card <- hand]
      pure a
    _ -> NeenaThurman <$> runMessage msg attrs
