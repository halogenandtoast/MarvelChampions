module Marvel.Treachery.Treacheries.UncannyResilience (
  uncannyResilience,
  UncannyResilience (..),
) where

import Marvel.Prelude

import Control.Monad ()
import Data.Maybe ()
import Data.Aeson ()
import Marvel.Attack ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Data.List ()
import Marvel.Id
import Marvel.Identity.Types ()
import Marvel.Projection ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

uncannyResilience :: TreacheryCard UncannyResilience
uncannyResilience = treachery UncannyResilience Cards.uncannyResilience

newtype UncannyResilience = UncannyResilience (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: remove each stunned and confused status card from each villain. If no
-- status cards were removed, this card gains surge.
-- [star] Boost: you are confused.
instance RunMessage UncannyResilience where
  runMessage msg t@(UncannyResilience attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      stunned <- selectList (CharacterWithStatus StunnedStatus)
      confused <- selectList (CharacterWithStatus ConfusedStatus)
      let villains xs = [VillainCharacter v | VillainCharacter v <- xs]
      for_ (villains stunned) (`removeStatus` StunnedStatus)
      for_ (villains confused) (`removeStatus` ConfusedStatus)
      pure $
        if null (villains stunned) && null (villains confused)
          then UncannyResilience (attrs & surgeL .~ True)
          else t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      confuseIdentity =<< getBoostedPlayer enemy
      pure t
    _ -> UncannyResilience <$> runMessage msg attrs
