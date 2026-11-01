module Marvel.Treachery.Treacheries.Headbutt (
  headbutt,
  Headbutt (..),
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
import Marvel.Id ()
import Marvel.Identity.Types ()
import Marvel.Projection ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

headbutt :: TreacheryCard Headbutt
headbutt = treachery Headbutt Cards.headbutt

newtype Headbutt = Headbutt (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: discard 1 card at random from your hand. If you are in hero form, take
-- damage equal to that card's printed cost; in alter-ego form, place that much threat on
-- Bulldozer's side scheme.
instance RunMessage Headbutt where
  runMessage msg t@(Headbutt attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardFor you attrs RandomFromHand 1 1
      pure $ Headbutt (attrs & resolverL ?~ you)
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      let cost = fromIntegral . max 0 $ sum [fromMaybe 0 (cdCost $ getCardDef c) | c <- cards]
      for_ (treacheryResolver attrs) \you -> when (cost > 0) do
        isHero <- identityMatches HeroIdentity you
        if isHero
          then damageIdentity you attrs (toDamage cost FromTreachery)
          else villainNamed "Bulldozer" >>= traverse_ \v -> traverse_ (`placeThreat` cost) =<< signatureSchemeOf v
      pure t
    _ -> Headbutt <$> runMessage msg attrs
