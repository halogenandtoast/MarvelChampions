module Marvel.Hero.Heroes.JubileeHero where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Runner
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats

jubileeHero :: HeroCard JubileeHero
jubileeHero = hero JubileeHero Cards.jubilee47001a (HP $ Static 9) (HandSize 5) (Thw 1) (Atk 1) (Def 2)
newtype JubileeHero = JubileeHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
instance HasAbilities JubileeHero where
  getAbilities a = [label "$label.jubileeLikeTotally" $ ability a 1 Resource IsSelf ExhaustCost (Pay $ ResourcePayment Wild)]
instance RunMessage JubileeHero where
  runMessage msg (JubileeHero attrs) = runQueueT $ JubileeHero <$> runMessage msg attrs
