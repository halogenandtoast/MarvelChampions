module Marvel.Hero.Heroes.Spectrum where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

spectrum :: HeroCard Spectrum
spectrum =
  hero
    Spectrum
    Cards.spectrum
    (HP $ Static 11)
    (HandSize 5)
    (Thw 1)
    (Atk 1)
    (Def 1)

-- | Energy Transformation — Forced Response: After you change to this form, choose a
-- facedown energy form upgrade → flip that card faceup to change to that energy form.
instance HasAbilities Spectrum where
  getAbilities a@(Spectrum attrs) =
    [ label "Energy Transformation" $
        limitedWindowAbility a 1 (W.IdentityChangedToForm W.After $ IdentityWithId $ heroIdentityId attrs) ForcedResponse IsSelf NoCost $
          runAbility a 1
    ]

newtype Spectrum = Spectrum (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Spectrum where
  runMessage msg h@(Spectrum attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      changeEnergyForm ident
      pure h
    _ -> Spectrum <$> runMessage msg attrs
