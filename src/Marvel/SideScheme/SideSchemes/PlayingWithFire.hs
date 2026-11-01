module Marvel.SideScheme.SideSchemes.PlayingWithFire where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.SideScheme.Types

playingWithFire :: SideSchemeCard PlayingWithFire
playingWithFire = sideScheme PlayingWithFire Cards.playingWithFire (Static 3)

newtype PlayingWithFire = PlayingWithFire (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PlayingWithFire where
  runMessage msg a@(PlayingWithFire attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      discardFromDeck you 3 attrs
      pure a
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let pcs = onlyPlayerCards cards
          n = fromIntegral $ sum $ map (length . printedResources . getCardDef) pcs
      for_ (headMay pcs >>= pcOwner) $ \you -> dealIndirectDamage you attrs n
      pure a
    _ -> PlayingWithFire <$> runMessage msg attrs
