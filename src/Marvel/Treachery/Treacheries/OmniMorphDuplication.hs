module Marvel.Treachery.Treacheries.OmniMorphDuplication (
  omniMorphDuplication,
  OmniMorphDuplication (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait
import Marvel.Ref (IsRef)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

omniMorphDuplication :: TreacheryCard OmniMorphDuplication
omniMorphDuplication = treachery OmniMorphDuplication Cards.omniMorphDuplication

newtype OmniMorphDuplication = OmniMorphDuplication (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | For each of Absorbing Man's traits: Ice, exhaust your identity; Metal, he gets a
-- tough status card and heals 1; Stone, he is given a facedown boost card; Wood, discard
-- 1 card at random from your hand.
instance RunMessage OmniMorphDuplication where
  runMessage msg t@(OmniMorphDuplication attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      villain <- selectOne ActiveVillain
      for_ villain \vid -> do
        let has tr = villainMatches (VillainWithTrait tr) vid
        whenM (has Ice) $ exhaust you
        whenM (has Metal) do
          enemyBecomesTough vid
          heal vid 1
        whenM (has Stone) $ dealBoost vid
        whenM (has Wood) $ discardFrom you RandomFromHand 1
      pure t
    _ -> OmniMorphDuplication <$> runMessage msg attrs
