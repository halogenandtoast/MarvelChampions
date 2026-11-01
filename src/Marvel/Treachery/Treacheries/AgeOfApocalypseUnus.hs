module Marvel.Treachery.Treacheries.AgeOfApocalypseUnus where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.Game.Source ()
import Marvel.Matchers ()
import Marvel.Id
import Marvel.Modifier (HasModifiersFor)
import Marvel.Query ()
import Marvel.SideScheme.SideSchemes.AgeOfApocalypseInfinites (genePoolThreat)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype AoaUnusTreachery = AoaUnusTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

infinitePrelate :: TreacheryCard AoaUnusTreachery
infinitePrelate = treachery AoaUnusTreachery Cards.infinitePrelate

instance RunMessage AoaUnusTreachery where
  runMessage msg e@(AoaUnusTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      -- The extra boost is established for the activation before it begins; the
      -- tough and healing clauses follow the activation, as printed.
      threat <- genePoolThreat
      villains <- maybe [] (: []) <$> villainNamed "Unus"
      for_ villains $ \vid -> do
        when (threat >= 9) $ additionalActivationBoost vid you
        villainActivatesAgainst vid you
        cardSpecific a "aoaInfinitePrelateAfterActivation" vid
      pure e
    CardSpecific target "aoaInfinitePrelateAfterActivation" (fromJSON -> Success vid) | target == toRef a -> do
      threat <- genePoolThreat
      when (threat >= 3) $ enemyBecomesTough (vid :: VillainId)
      when (threat >= 6) $ heal vid 3
      pure e
    _ -> AoaUnusTreachery <$> runMessage msg a
