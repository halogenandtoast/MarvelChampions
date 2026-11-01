module Marvel.SideScheme.SideSchemes.NextEvolutionBlackTomCassidy where

import Marvel.Minion.Cards qualified as CardsWillow
import Marvel.Prelude
import Marvel.Status ()
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability ()
import Marvel.Attack ()
import Marvel.Cost ()
import Marvel.Count ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Difficulty ()
import Marvel.Game.Source ()
import Marvel.Identity.Types
import Marvel.Keyword ()
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Message.Lifted ( RunMessage(..), runQueueT )
import Marvel.Modifier
    ( HasModifiersFor(getModifiersFor), Modifier(KeywordModifier) )
import Marvel.NextEvolution.Juggernaut ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Window qualified as W ()
import Marvel.Card
import Marvel.Entity ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types


import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()


newtype BlackTomCassidySideScheme = BlackTomCassidySideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40134 :: SideSchemeCard BlackTomCassidySideScheme
ne40134 = sideScheme BlackTomCassidySideScheme Cards.ne40134 (Static 2)


instance HasModifiersFor BlackTomCassidySideScheme where
  getModifiersFor _ (MinionRef mid) _ = do
    willow <- minionMatches (MinionIs CardsWillow.ne40133) mid
    pure [KeywordModifier Keyword.Surge | willow]
  getModifiersFor _ _ _ = pure []

instance RunMessage BlackTomCassidySideScheme where
  runMessage msg (BlackTomCassidySideScheme attrs) = runQueueT $ case msg of
    _ -> BlackTomCassidySideScheme <$> runMessage msg attrs
