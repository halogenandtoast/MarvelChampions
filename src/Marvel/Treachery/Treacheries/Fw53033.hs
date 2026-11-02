{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Treachery.Treacheries.Fw53033 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Treachery.Types

fw53033 :: TreacheryCard Fw53033
fw53033 = treachery Fw53033 Cards.fw53033

newtype Fw53033 = Fw53033 (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw53033 where
  runMessage msg a@(Fw53033 attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      cards <- filter (cardMatch $ CardWithType MinionType <> CardWithTrait T.SerpentSociety) <$> getEncounterDiscard
      shuffleIntoEncounterDeck cards
      pure a
    _ -> Fw53033 <$> runMessage msg attrs
