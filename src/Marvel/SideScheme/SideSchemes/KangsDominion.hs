module Marvel.SideScheme.SideSchemes.KangsDominion (
  kangsDominion,
  KangsDominion (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Name
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

kangsDominion :: SideSchemeCard KangsDominion
kangsDominion = sideScheme (KangsDominion . (`With` Meta Nothing)) Cards.kangsDominion (PerPlayer 3)

-- | The player whose thwart defeats it.
newtype Meta = Meta {defeatedBy :: Maybe IdentityId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype KangsDominion = KangsDominion (Attrs SideScheme `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasCardCode KangsDominion where
  toCardCode (KangsDominion (a `With` _)) = toCardCode a

instance IsSideScheme KangsDominion where
  toSideSchemeAttrs (KangsDominion (a `With` _)) = a

-- | Kang cannot take damage: the villain titled Kang in its game area.
instance HasModifiersFor KangsDominion where
  getModifiersFor _ (VillainRef vid) (KangsDominion (a `With` _)) = do
    here <- getAreaOfTarget (sideSchemeId a)
    there <- getAreaOfTarget vid
    kang <- (== Just "Kang") . fmap (title . cdName) <$> getsGame (`abilityCardDef` VillainRef vid)
    pure [CannotTakeDamage | kang, here == there]
  getModifiersFor _ _ _ = pure []

-- | When Defeated: deal the player who defeated this scheme an encounter card.
instance RunMessage KangsDominion where
  runMessage msg s@(KangsDominion (attrs `With` meta)) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      SideSchemeThwarted source _ -> do
        player <- maybe getActivePlayerId pure =<< getSourceController source
        attrs' <- runMessage msg attrs
        pure . KangsDominion $ attrs' `With` Meta (Just player)
      DefeatSideScheme -> do
        dealEncounterCard =<< maybe getActivePlayerId pure (defeatedBy meta)
        pure s
      _ -> KangsDominion . (`With` meta) <$> runMessage msg attrs
    _ -> KangsDominion . (`With` meta) <$> runMessage msg attrs
