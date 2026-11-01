-- | The treachery of the Legions of Hel modular set (The Mad Titan's Shadow).
module Marvel.Treachery.Treacheries.LegionsOfHel (
  noPlaceForTheLiving,
  NoPlaceForTheLiving (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Keyword (Keyword (Permanent))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

noPlaceForTheLiving :: TreacheryCard NoPlaceForTheLiving
noPlaceForTheLiving = treachery NoPlaceForTheLiving Cards.noPlaceForTheLiving

newtype NoPlaceForTheLiving = NoPlaceForTheLiving (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: each player, the first player first, must choose to either discard
-- the upgrade or support they control with the highest cost (a tie: which), or take
-- damage equal to the total number of upgrades and supports they control. With none to
-- discard, they take that damage (0 with none at all).
instance RunMessage NoPlaceForTheLiving where
  runMessage msg t@(NoPlaceForTheLiving attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      players <- getPlayers
      for_ players \p -> do
        upgrades <- selectList (UpgradeControlledBy $ IdentityWithId p)
        supports <- selectList (SupportControlledBy $ IdentityWithId p)
        let n = fromIntegral (length upgrades + length supports)
        -- a permanent card cannot be discarded: the highest cost of the others (RR
        -- Permanent)
        discardables <- filterM discardable (map UpgradeRef upgrades <> map SupportRef supports)
        costed <- for discardables \c -> (c,) . fromIntegral . max 0 . fromMaybe 0 . (cdCost =<<) <$> getsGame (`abilityCardDef` c)
        let highest = extremes MostThreat costed
        chooseOne p $
          [targeting c (discardControlled c) | c <- highest]
            <> [labeled (countLabel "takeDamage" n) $ when (n > 0) $ damageIdentity p attrs (toDamage n FromAbility)]
      pure t
    _ -> NoPlaceForTheLiving <$> runMessage msg attrs
   where
    discardable c = getsGame \g -> maybe True (notElem Permanent . cdKeywords) (abilityCardDef g c)
