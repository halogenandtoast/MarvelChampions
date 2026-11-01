module Marvel.Hero.Heroes.Nc48001a where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Hero.Runner
import Marvel.Hero.Cards qualified as Cards

nc48001a :: HeroCard Nc48001a
nc48001a = hero Nc48001a Cards.nc48001a (HP $ Static 9) (HandSize 5) (Thw 2) (Atk 1) (Def 3)

newtype Nc48001a = Nc48001a (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Nc48001a where
  getAbilities a = [label "$label.rapidTeleportation" $ limitedAbility a 1 (PerPhase 1) Action
    (IsSelf <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardWithTitle "Bamf!"))
    (ResourceCost Nothing) (runAbility a 1)]

instance RunMessage Nc48001a where
  runMessage msg a@(Nc48001a attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithTitle "Bamf!")
      chooseOne you [targeting card (addToHand you card) | card <- cards]
      pure a
    _ -> Nc48001a <$> runMessage msg attrs
