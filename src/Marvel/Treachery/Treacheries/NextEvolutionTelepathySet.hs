module Marvel.Treachery.Treacheries.NextEvolutionTelepathySet where

import Marvel.Prelude
import Marvel.Ability ( HasAbilities )
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier ( HasModifiersFor )
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.NextEvolution.Sinister ()

import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types


import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype TelepathySetTreachery = TelepathySetTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40162 :: TreacheryCard TelepathySetTreachery
ne40162 = treachery TelepathySetTreachery Cards.ne40162

instance RunMessage TelepathySetTreachery where
  runMessage msg card@(TelepathySetTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      aerial <- selectAny $ ActiveVillain <> VillainWithTrait Trait.Aerial
      discardFrom you RandomFromHand (if aerial then 2 else 1)
      pure card
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      case enemy of
        EnemyVillainId vid -> whenM (isJust <$> getVillainAttack vid) $ villainStep vid (VillainAttackGains Keyword.Overkill)
        _ -> pure ()
      pure card
    _ -> TelepathySetTreachery <$> runMessage msg a
