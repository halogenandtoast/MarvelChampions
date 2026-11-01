module Marvel.Treachery.Treacheries.TheWarsBeenBrought where

import Marvel.Prelude
import Marvel.Card
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand, PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

theWarsBeenBrought :: TreacheryCard TheWarsBeenBrought
theWarsBeenBrought = treachery TheWarsBeenBrought Cards.theWarsBeenBrought

newtype TheWarsBeenBrought = TheWarsBeenBrought (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheWarsBeenBrought where
  runMessage msg t@(TheWarsBeenBrought attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      hand <- projectMap PlayerIdentityHand unHand you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      supports <- selectList (SupportControlledBy $ IdentityWithId you)
      upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
      controlled <- fmap catMaybes $ for (IdentityRef you : map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades) $ \target -> getsGame (`abilityCardDef` target)
      let n = count (== Wild) $ concatMap printedResources (map getCardDef (hand <> discard) <> controlled)
      discardTopOfEncounterDeck n
      pure t
    _ -> TheWarsBeenBrought <$> runMessage msg attrs
