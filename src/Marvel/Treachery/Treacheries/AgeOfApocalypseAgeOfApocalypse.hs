module Marvel.Treachery.Treacheries.AgeOfApocalypseAgeOfApocalypse where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Modifier (HasModifiersFor)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype AgeOfApocalypseTreachery = AgeOfApocalypseTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

worldwideCrisis :: TreacheryCard AgeOfApocalypseTreachery
worldwideCrisis = treachery AgeOfApocalypseTreachery Cards.worldwideCrisis

instance RunMessage AgeOfApocalypseTreachery where
  runMessage msg e@(AgeOfApocalypseTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      mission <- getMission
      chooseOne you $
        [labeled "$label.aoaMissionThreeThreat" (placeThreat m.missionScheme 3) | m <- maybe [] (: []) mission, not m.missionFinished]
        <> [labeled "$label.aoaWorldwideCrisisDamage" (damageIdentity you a (toDamage 1 FromAbility) >> gainSurge a)]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      mission <- getMission
      for_ mission $ \m -> unless m.missionFinished $ placeThreat m.missionScheme 1
      dealBoost (enemyRef enemy)
      pure e
    _ -> AgeOfApocalypseTreachery <$> runMessage msg a
