module Marvel.AlterEgo.AlterEgos.ShuriAlterEgo where
import Marvel.Prelude
import Marvel.Id ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Discard ()
import Marvel.Deck ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

shuriAlterEgo :: AlterEgoCard ShuriAlterEgo
shuriAlterEgo = alterEgo ShuriAlterEgo Cards.shuriAlterEgo (HP $ Static 11) (HandSize 6) (Rec 4) [Cards.tChallasShadowShuri]
newtype ShuriAlterEgo = ShuriAlterEgo (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ShuriAlterEgo where
  runMessage msg u@(ShuriAlterEgo attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithType UpgradeType <> CardMatchesAny [CardWithTrait T.BlackPanther, CardWithTrait T.Tech]) (SearchTarget $ toRef attrs) ShuffleBackIn
      pure u
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) -> do
      for_ cards $ \card -> do
        addToHand (alterEgoIdentityId attrs) card
        playCardWithReduction (alterEgoIdentityId attrs) 2 card
      pure u
    _ -> ShuriAlterEgo <$> runMessage msg attrs

instance HasAbilities ShuriAlterEgo where
  getAbilities u@(ShuriAlterEgo _attrs) = [limited (PerRound 1) $ ability u 1 AlterEgoAction IsSelf ExhaustCost (runAbility u 1)]
