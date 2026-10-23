module Marvel.SideScheme.SideSchemes.TechTheft (
  techTheft,
  TechTheft (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

techTheft :: SideSchemeCard TechTheft
techTheft = sideScheme TechTheft Cards.techTheft (PerPlayer 2)

newtype TechTheft = TechTheft (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Treat the printed text box of each Tech player card as if it were blank: the printed
-- trait, read straight off the card, as whether it is blank decides its modifiers.
instance HasModifiersFor TechTheft where
  getModifiersFor _ target _ = case target of
    UpgradeRef _ -> blankIfTech
    SupportRef _ -> blankIfTech
    AllyRef _ -> blankIfTech
    _ -> pure []
   where
    blankIfTech = do
      def <- getsGame (`abilityCardDef` target)
      pure [Blank | maybe False (member Tech . cdTraits) def]

instance RunMessage TechTheft where
  runMessage msg (TechTheft attrs) = TechTheft <$> runMessage msg attrs
