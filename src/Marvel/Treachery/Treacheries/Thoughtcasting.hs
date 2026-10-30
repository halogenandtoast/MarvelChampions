module Marvel.Treachery.Treacheries.Thoughtcasting (
  thoughtcasting,
  Thoughtcasting (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

thoughtcasting :: TreacheryCard Thoughtcasting
thoughtcasting = treachery Thoughtcasting Cards.thoughtcasting

newtype Thoughtcasting = Thoughtcasting (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

printedCost :: PlayerCard -> Natural
printedCost = maybe 0 fromIntegral . cdCost . getCardDef

-- | Discard a card from your hand with the highest cost (the player picks among those
-- tied); its printed cost is placed as threat on the main scheme in alter-ego form, or
-- taken as damage in hero form.
instance RunMessage Thoughtcasting where
  runMessage msg t@(Thoughtcasting attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      hand <- projectMap PlayerIdentityHand unHand identityId
      hero <- member identityId <$> select HeroIdentity
      mainSchemes <- selectList EncounterMainScheme
      let highest = maximum (0 : map printedCost hand)
          consequence c
            | hero = damageCharacter identityId attrs (toDamage (printedCost c) FromTreachery)
            | otherwise = for_ mainSchemes (`placeThreat` printedCost c)
      chooseOnePrompted identityId "$label.discardHighestCost"
        [ targeting c (discardCard c >> consequence c)
        | c <- hand
        , printedCost c == highest
        ]
      pure t
    _ -> Thoughtcasting <$> runMessage msg attrs
