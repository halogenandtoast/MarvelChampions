module Marvel.SideScheme.SideSchemes.NewRecruits where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

newRecruits :: SideSchemeCard NewRecruits
newRecruits = sideScheme NewRecruits Cards.newRecruits (PerPlayer 2)

newtype NewRecruits = NewRecruits (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage NewRecruits where
  runMessage msg e@(NewRecruits attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players $ \you -> cardSpecific attrs "recruit" you
      pure e
    CardSpecific (isTarget attrs -> True) "recruit" (fromJSON -> Success you) -> do
      cards <- filter (cardMatch $ CardWithTrait T.New) <$> getSetAsidePlayerCards
      chooseOne you [targeting c $ takeControlOf you c | c <- cards]
      pure e
    _ -> NewRecruits <$> runMessage msg attrs
