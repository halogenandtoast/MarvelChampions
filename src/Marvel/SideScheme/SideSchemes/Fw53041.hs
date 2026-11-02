{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.SideScheme.SideSchemes.Fw53041 where
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
import Marvel.SideScheme.Types

import Marvel.FalconWinter.Helpers
fw53041 :: SideSchemeCard Fw53041
fw53041 = sideScheme Fw53041 Cards.fw53041 (Static 4)

newtype Fw53041 = Fw53041 (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw53041 where
  runMessage msg a@(Fw53041 attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      you <- getYou
      cards <- filter (cardMatch $ CardWithType AttachmentType <> CardWithTrait T.Tech) <$> ((<>) <$> getEncounterDeck <*> getEncounterDiscard)
      if null cards then cardSpecific attrs "countTech" () else
        chooseOrRunOne you [targeting card (revealEncounterCard you card >> cardSpecific attrs "countTech" ()) | card <- cards]
      shuffleEncounterDeck
      pure a
    CardSpecific (isTarget attrs -> True) "countTech" _ -> do
      n <- techCardsInPlay
      placeThreatOn attrs (sideSchemeId attrs) n
      pure a
    _ -> Fw53041 <$> runMessage msg attrs
