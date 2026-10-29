module Marvel.SideScheme.SideSchemes.DeadlyLightShow (deadlyLightShow, DeadlyLightShow (..)) where

import Marvel.Prelude
import Marvel.Damage
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Fixed three threat, plus Hinder one per player, and a crisis icon.
deadlyLightShow :: SideSchemeCard DeadlyLightShow
deadlyLightShow = sideSchemeWith DeadlyLightShow Cards.deadlyLightShow (Static 3) (crisisL .~ True)

newtype DeadlyLightShow = DeadlyLightShow (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage DeadlyLightShow where
  runMessage msg (DeadlyLightShow attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \you -> damageIdentity you attrs (toDamage 1 FromAbility)
      DeadlyLightShow <$> runMessage msg attrs
    _ -> DeadlyLightShow <$> runMessage msg attrs
