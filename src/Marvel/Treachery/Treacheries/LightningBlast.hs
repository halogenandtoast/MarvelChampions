module Marvel.Treachery.Treacheries.LightningBlast (
  lightningBlast,
  LightningBlast (..),
) where

import Marvel.Prelude

import Control.Monad ()
import Data.Maybe ()
import Data.Aeson ()
import Marvel.Attack ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Data.List ()
import Marvel.Id ()
import Marvel.Identity.Types ()
import Marvel.Projection ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Ref ()
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

lightningBlast :: TreacheryCard LightningBlast
lightningBlast = treachery (LightningBlast . (`With` Meta False False)) Cards.lightningBlast

data Meta = Meta {attacked :: Bool, defended :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LightningBlast = LightningBlast (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery LightningBlast where
  toTreacheryAttrs (LightningBlast (attrs `With` _)) = attrs

-- | When Revealed (Alter-Ego): place 3 threat on the side scheme with the most threat.
-- (Hero): Thunderball attacks you. If this attack is undefended, place 3 threat on
-- Thunderball's side scheme.
instance RunMessage LightningBlast where
  runMessage msg t@(LightningBlast (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      if isHero
        then villainNamed "Thunderball" >>= traverse_ \v -> do
          enemyAttacks v you
          cardSpecific attrs "undefended" v
        else withSideSchemeByThreat MostThreat AnySideScheme (`placeThreat` 3)
      pure t
    VillainMessage v VillainAttacked -> ours v t \m -> m {attacked = True}
    VillainMessage v (VillainDefendedBy _) -> ours v t \m -> m {defended = True}
    CardSpecific (isTarget attrs -> True) "undefended" (fromJSON -> Success v) -> do
      when (attacked meta && not (defended meta)) $ traverse_ (`placeThreat` 3) =<< signatureSchemeOf v
      pure t
    _ -> LightningBlast . (`With` meta) <$> runMessage msg attrs
   where
    -- Thunderball's attack, while this card resolves
    ours v t' f = do
      isThunderball <- (== Just v) <$> villainNamed "Thunderball"
      pure $ if isThunderball then LightningBlast (attrs `With` f meta) else t'
