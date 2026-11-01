module Marvel.Treachery.Treacheries.AgeOfApocalypseDarkBeast where

import Marvel.Prelude
import Marvel.Game.Source ()
import Marvel.AgeOfApocalypse.Settings ()
import Marvel.Card
import Marvel.Entity
import Marvel.Id (EnemyId (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype AoaDarkBeastTreachery = AoaDarkBeastTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

evilGenius :: TreacheryCard AoaDarkBeastTreachery
evilGenius = treachery AoaDarkBeastTreachery Cards.evilGenius

instance RunMessage AoaDarkBeastTreachery where
  runMessage msg e@(AoaDarkBeastTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      enemies <- selectList (EnemyWithTitle "Dark Beast")
      let villains = [vid | EnemyVillainId vid <- enemies]
      for_ villains \villain -> if hero
        then villainStep villain (VillainAttacksWithAdditionalBoostCards you 1)
        else villainSchemesAgainst villain you >> enemyBecomesTough villain
      pure e
    _ -> AoaDarkBeastTreachery <$> runMessage msg attrs
