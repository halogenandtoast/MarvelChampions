module Marvel.Hero.Heroes.ArchangelHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

archangelHero :: HeroCard ArchangelHero
archangelHero = hero ArchangelHero Cards.archangelHero (HP $ Static 12) (HandSize 5) (Thw 0) (Atk 2) (Def 3)

newtype ArchangelHero = ArchangelHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ArchangelHero where
  getAbilities a@(ArchangelHero attrs) =
    [ label "Angel of Death" $ limited (PerPhase 1) $
        limitedWindowAbility a 1 (W.PlaysEvent W.After (IdentityWithId $ heroIdentityId attrs) (CardWithTrait Trait.Aerial <> CardWithPrintedCostAtLeast 1))
          Response (IsSelf <> EnemyExists AnyEnemy) NoCost (runAbility a 1)
    ]

instance RunMessage ArchangelHero where
  runMessage msg h@(ArchangelHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [def | W.EventPlayed _ _ def <- windows] \def ->
        chooseDamage you attrs (toDamage (fromIntegral $ max 0 $ fromMaybe 0 $ cdCost def) FromAbility) AnyEnemy
      pure h
    _ -> ArchangelHero <$> runMessage msg attrs
