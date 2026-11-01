module Marvel.Treachery.Treacheries.Cable where

import Marvel.Prelude
import Data.List ()
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Status ()
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Keyword qualified as Keyword ()
import {-# SOURCE #-} Marvel.Game ()

import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Stats ()
import Marvel.GameValue ()
import Marvel.Hand ()

cable40035 :: TreacheryCard Cable40035
cable40035 = treachery Cable40035 Cards.cable40035

newtype Cable40035 = Cable40035 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40035 where
  runMessage msg a@(Cable40035 attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId attrs -> do
      n <- sideSchemesInVictoryDisplay
      placeThreatOnEach attrs (2 + n) MainScheme
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      confuseIdentity you
      pure a
    _ -> Cable40035 <$> runMessage msg attrs

cable40036 :: TreacheryCard Cable40036
cable40036 = treachery Cable40036 Cards.cable40036

newtype Cable40036 = Cable40036 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40036 where
  runMessage msg a@(Cable40036 attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      n <- sideSchemesInVictoryDisplay
      damageIdentity you attrs (toDamage (2 + n) FromAbility)
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      stunIdentity you
      pure a
    _ -> Cable40036 <$> runMessage msg attrs
