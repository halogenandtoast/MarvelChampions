module Marvel.Treachery.Treacheries.YoureDeadMeat (
  youreDeadMeat,
  YoureDeadMeat (..),
) where

import Marvel.Prelude

import Control.Monad ()
import Data.Maybe ()
import Data.Aeson ()
import Marvel.Attack ()
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Data.List ()
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDefeated))
import Marvel.Projection (projectP)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

youreDeadMeat :: TreacheryCard YoureDeadMeat
youreDeadMeat = treachery YoureDeadMeat Cards.youreDeadMeat

newtype YoureDeadMeat = YoureDeadMeat (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: deal 1 damage to the hero or ally with the fewest remaining hit
-- points (you choose among those tied). If that character is defeated this way, place 3
-- threat on Wrecker's side scheme.
instance RunMessage YoureDeadMeat where
  runMessage msg t@(YoureDeadMeat attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      characters <- selectList HeroOrAllyWithFewestRemainingHitPoints
      unless (null characters) $ chooseOrRunOne you
        [ targeting c do
            case c of
              IdentityCharacter i -> damageIdentity i attrs (toDamage 1 FromTreachery)
              _ -> damageCharacter c attrs (toDamage 1 FromTreachery)
            cardSpecific attrs "defeated" c
        | c <- characters
        ]
      pure t
    CardSpecific (isTarget attrs -> True) "defeated" (fromJSON -> Success c) -> do
      gone <- case c of
        IdentityCharacter i -> isDefeated i
        AllyCharacter a -> selectNone (AllyWithId a)
        _ -> pure False
      when gone $
        villainNamed "Wrecker" >>= traverse_ \w -> traverse_ (`placeThreat` 3) =<< signatureSchemeOf w
      pure t
    _ -> YoureDeadMeat <$> runMessage msg attrs
   where
    isDefeated i = selectNone (IdentityWithId i) >>= \case
      True -> pure True
      False -> projectP PlayerIdentityDefeated id i
