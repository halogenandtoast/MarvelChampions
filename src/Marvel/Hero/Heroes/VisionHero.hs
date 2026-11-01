module Marvel.Hero.Heroes.VisionHero where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Types
import Marvel.Keyword (Keyword (MassForm))
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Hero.Runner ()
visionHero :: HeroCard VisionHero
visionHero = hero VisionHero Cards.visionHero (HP $ Static 11) (HandSize 5) (Thw 2) (Atk 0) (Def 0)

newtype VisionHero = VisionHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities VisionHero where
  getAbilities a@(VisionHero attrs) =
    [ label "$label.densityManipulation" $
        limitedAbility a 1 (PerRound 1) Action (IsSelf <> SelfMatches IdentityCanChangeForm <> UpgradeExists (UpgradeWithKeyword MassForm <> UpgradeControlledBy (IdentityWithId $ heroIdentityId attrs))) NoCost (runAbility a 1)
    ]

instance RunMessage VisionHero where
  runMessage msg h@(VisionHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      forms <- selectList (UpgradeWithKeyword MassForm <> UpgradeControlledBy (IdentityWithId you))
      for_ (headMay forms) (flipMassForm you)
      pure h
    _ -> VisionHero <$> runMessage msg attrs
