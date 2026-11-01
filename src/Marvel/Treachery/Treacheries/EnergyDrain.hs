module Marvel.Treachery.Treacheries.EnergyDrain where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
    ( IdentityMatcher(UnexhaustedIdentity, HeroIdentity),
      identityMatches )
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Damage
import Marvel.Resource


energyDrain :: TreacheryCard EnergyDrain
energyDrain = treachery EnergyDrain Cards.energyDrain

newtype EnergyDrain = EnergyDrain (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage EnergyDrain where
  runMessage msg e@(EnergyDrain attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      let cost = MultiResourceCost [Just Energy, Just Energy]
      payable <- canAffordCost you (toSource attrs) cost
      hero <- identityMatches HeroIdentity you
      ready <- identityMatches UnexhaustedIdentity you
      let alternatives =
            [labeled (countLabel "takeDamage" 3) (damageIdentity you attrs $ toDamage 3 FromTreachery) | hero]
            <> [labeled "$label.exhaustYourIdentity" (exhaust you) | not hero && ready]
          choices = [labeled (countLabel "spendEnergyResources" 2) (payForTreachery you cost) | payable] <> alternatives
      unless (null choices) $ chooseOrRunOne you choices
      pure e
    _ -> EnergyDrain <$> runMessage msg attrs
