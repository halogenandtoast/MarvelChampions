module Marvel.Treachery.Treacheries.NextEvolutionBlackTomCassidy where

import Marvel.Minion.Cards qualified as CardsWillow
import Marvel.Minion.Types ()
import Marvel.Prelude
import Marvel.Id
import Marvel.Status ()
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability ( HasAbilities )
import Marvel.Attack ()
import Marvel.Cost ()
import Marvel.Count ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Difficulty ()
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
    ( Message(CardSpecific, TreacheryMessage, CheckWindows),
      RunMessage(..),
      TreacheryMessage(RevealTreachery),
      runQueueT,
      cardSpecific,
      enemyAttacks,
      searchAndRevealEncounterCard )
import Marvel.Modifier ( HasModifiersFor )
import Marvel.NextEvolution.Juggernaut ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types


import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()


newtype BlackTomCassidyTreachery = BlackTomCassidyTreachery (Attrs Treachery `With` [IdentityId])
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ne40135 :: TreacheryCard BlackTomCassidyTreachery
ne40135 = treachery (BlackTomCassidyTreachery . (`With` [])) Cards.ne40135


instance IsTreachery BlackTomCassidyTreachery where
  toTreacheryAttrs (BlackTomCassidyTreachery (attrs `With` _)) = attrs
instance IsRef BlackTomCassidyTreachery where
  toRef = toRef . toTreacheryAttrs
instance HasAbilities BlackTomCassidyTreachery
instance HasModifiersFor BlackTomCassidyTreachery

instance RunMessage BlackTomCassidyTreachery where
  runMessage msg a@(BlackTomCassidyTreachery (attrs `With` attacked)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      players <- getPlayers
      engaged <- concatMapM (\player -> do
        willows <- selectList (MinionIs CardsWillow.ne40133 <> MinionEngagedWith (IdentityWithId player))
        pure [(mid,player) | mid <- willows]) players
      for_ engaged $ \(mid,player) -> enemyAttacks mid player
      cardSpecific attrs "checkAttacked" you
      pure $ BlackTomCassidyTreachery (attrs `With` [])
    CheckWindows windows -> do
      players <- fmap catMaybes $ for windows $ \case
        W.Window W.When (W.EnemyAttack (EnemyMinionId mid) you) -> do
          willow <- minionMatches (MinionIs CardsWillow.ne40133) mid
          pure [you | willow] >>= pure . headMay
        _ -> pure Nothing
      attrs' <- runMessage msg attrs
      pure $ BlackTomCassidyTreachery (attrs' `With` (attacked <> players))
    CardSpecific (isTarget attrs -> True) "checkAttacked" (fromJSON -> Success you) -> do
      unless (you `elem` attacked) $ searchAndRevealEncounterCard you (CardWithTitle "Creeping Willow")
      pure a
    _ -> BlackTomCassidyTreachery . (`With` attacked) <$> runMessage msg attrs
