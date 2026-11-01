module Marvel.Support.Supports.DangerRoom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck (unDeck)
import Marvel.Discard (unDiscard)
import Marvel.Entity
import Marvel.Game.Source (getPlayTargets)
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Question
import Marvel.Query ()
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

dangerRoom :: SupportCard DangerRoom
dangerRoom = support DangerRoom Cards.dangerRoom

newtype DangerRoom = DangerRoom (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities DangerRoom where
  getAbilities a =
    [limitedWindowAbility a 1 (W.AllyEntersPlay W.After $ AllyWithTrait Trait.XMen)
      AlterEgoResponse (ForAnyPlayer <> AnyCriteria [OwnsThis, SelfMatches (IdentityWithTrait Trait.Mutant)]) ExhaustCost (runAbility a 1)]

instance RunMessage DangerRoom where
  runMessage msg a@(DangerRoom attrs) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let entrants = [aid | W.AllyEnteredPlay aid <- windows]
      eligible <- filterM (allyMatches $ AllyWithTrait Trait.XMen) entrants
      for_ eligible $ \entrant -> do
        deck <- projectMap PlayerIdentityDeck unDeck you
        discard <- projectMap PlayerIdentityDiscard unDiscard you
        let training = CardWithType UpgradeType <> CardWithTrait Trait.Training
            candidates = filter (cardMatch training) (deck <> discard)
        legal <- filterM (fmap (maybe False (elem $ toTarget entrant)) . getPlayTargets you) candidates
        focusing (map PlayerCard $ deck <> discard) $
          chooseOne you $
            [labeled "$label.noMatchingCardsFound" (pure ()) | null legal]
              <> [targeting card (putIntoPlayOn you card $ toTarget entrant) | card <- legal]
      shuffleDeck you
      pure a
    _ -> DangerRoom <$> runMessage msg attrs
