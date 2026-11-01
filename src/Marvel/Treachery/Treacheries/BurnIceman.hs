module Marvel.Treachery.Treacheries.BurnIceman where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Treachery.Types

burnIceman :: TreacheryCard BurnIceman
burnIceman = treachery (BurnIceman . (`With` Nothing)) Cards.burnIceman

newtype BurnIceman = BurnIceman (Attrs Treachery `With` Maybe EnemyId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery BurnIceman where
  toTreacheryAttrs (BurnIceman (attrs `With` _)) = attrs

instance RunMessage BurnIceman where
  runMessage msg a@(BurnIceman (attrs `With` boosted)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      present <- selectAny (MinionIs Cards.pyroIceman)
      discardFromDeck you (if present then 3 else 2) attrs
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      discardFromDeck you 1 attrs
      pure $ BurnIceman (attrs `With` Just enemy)
    Boost inner@(WithDiscarded (isTarget attrs -> True) FromDeck _) -> runMessage inner a
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let pcs = onlyPlayerCards cards
          n = fromIntegral $ sum [length $ printedResources $ getCardDef c | c <- pcs]
      case boosted of
        Just enemy -> addEnemyBoost enemy n
        Nothing -> for_ (headMay pcs >>= pcOwner) $ \you -> dealIndirectDamage you attrs n
      pure a
    _ -> (\updated -> BurnIceman (updated `With` boosted)) <$> runMessage msg attrs
