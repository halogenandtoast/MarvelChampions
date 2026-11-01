module Marvel.Hero.Heroes.IcemanHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers
import Marvel.Hero.Runner
import Marvel.Stats

icemanHero :: HeroCard IcemanHero
icemanHero = hero IcemanHero Cards.icemanHero (HP $ Static 11) (HandSize 5) (Thw 1) (Atk 2) (Def 2)

newtype IcemanHero = IcemanHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities IcemanHero where
  getAbilities a@(IcemanHero attrs) =
    [ label "$label.freeze" $ limitedWindowAbility a 1
        (W.MakesBasicAttackAgainst W.When (IdentityWithId $ heroIdentityId attrs) AnyEnemy)
        Interrupt (IsSelf <> ExtendedCardExists (InSetAsideOf AnyIdentity $ BasicCardMatches $ CardIs Cards.frostbite)) NoCost (runAbility a 1)
    , label "$label.freeze" $ limitedWindowAbility a 2
        (W.MakesBasicDefense W.When (IdentityWithId $ heroIdentityId attrs) AnyEnemy)
        Interrupt (IsSelf <> ExtendedCardExists (InSetAsideOf AnyIdentity $ BasicCardMatches $ CardIs Cards.frostbite)) NoCost (runAbility a 2)
    ]

instance RunMessage IcemanHero where
  runMessage msg a@(IcemanHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) n windows _ | n `elem` [1,2] -> do
      for_ (headMay $ concatMap (\case W.MadeBasicAttackAgainst _ enemy -> [enemy]; W.MadeBasicDefense _ enemy -> [enemy]; _ -> []) windows) $ attachFrostbite you
      pure a
    CardSpecific (IdentityRef you) "icemanDelayedFrostbite" (fromJSON -> Success enemy) | you == heroIdentityId attrs -> a <$ attachFrostbite you enemy
    _ -> IcemanHero <$> runMessage msg attrs
