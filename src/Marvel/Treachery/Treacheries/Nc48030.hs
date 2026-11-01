module Marvel.Treachery.Treacheries.Nc48030 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window ()
import Marvel.Treachery.Types
import Marvel.Treachery.Cards qualified as Cards

nc48030 :: TreacheryCard Nc48030
nc48030 = treachery Nc48030 Cards.nc48030

newtype Nc48030 = Nc48030 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Nc48030 where
  runMessage msg a@(Nc48030 attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      alterEgo <- selectAny $ IdentityWithId you <> AlterEgoIdentity
      azazel <- selectOne $ MinionIs Minions.nc48027
      case azazel of
        Just mid -> do
          engaged <- getMinionEngagedPlayer mid
          when (engaged /= Just you) $ engageMinion mid you
          minionStep mid (RevealMinion you)
        Nothing -> do
          cards <- findEncounterCards (CardIs Minions.nc48027)
          for_ (headMay cards) $ \card -> do
            deck <- getEncounterDeck
            revealFoundEncounterCard you card
            when (card `elem` deck) shuffleEncounterDeck
      when alterEgo $ cardSpecific attrs "scheme" you
      pure a
    CardSpecific (isTarget attrs -> True) "scheme" (fromJSON -> Success you) -> do
      azazel <- selectOne $ MinionIs Minions.nc48027
      for_ azazel $ \mid -> minionSchemesAgainst mid you
      pure a
    _ -> Nc48030 <$> runMessage msg attrs
