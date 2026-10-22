module Marvel.SideScheme.SideSchemes.CrossbonesAssault (
  crossbonesAssault,
  CrossbonesAssault (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

crossbonesAssault :: SideSchemeCard CrossbonesAssault
crossbonesAssault = sideScheme (CrossbonesAssault . (`With` Meta Nothing)) Cards.crossbonesAssault (PerPlayer 2)

-- | The player whose thwarting last removed threat from it: who defeats it.
newtype Meta = Meta {thwartedBy :: Maybe IdentityId}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype CrossbonesAssault = CrossbonesAssault (Attrs SideScheme `With` Meta)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsSideScheme CrossbonesAssault where
  toSideSchemeAttrs (CrossbonesAssault (attrs `With` _)) = attrs

instance HasCardCode CrossbonesAssault where
  toCardCode = toCardCode . toSideSchemeAttrs

-- | When Defeated: Crossbones activates against the player who defeated this scheme:
-- he attacks a hero, and schemes against an alter-ego.
instance RunMessage CrossbonesAssault where
  runMessage msg s@(CrossbonesAssault (attrs `With` meta)) = runQueueT $ case msg of
    SideSchemeMessage ident (SideSchemeThwarted source _) | ident == sideSchemeId attrs -> do
      who <- getSourceController source
      attrs' <- runMessage msg attrs
      pure $ CrossbonesAssault (attrs' `With` Meta (who <|> thwartedBy meta))
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      you <- maybe getActivePlayerId pure (thwartedBy meta)
      villain <- selectOne ActiveVillain
      for_ villain \vid -> do
        hero <- identityMatches HeroIdentity you
        if hero then enemyAttacks vid you else villainSchemes vid
      pure s
    _ -> CrossbonesAssault . (`With` meta) <$> runMessage msg attrs
