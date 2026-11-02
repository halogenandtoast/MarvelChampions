module Marvel.Hero.Heroes.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.WonderMan.Helpers
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC
import Marvel.Hero.Runner ()
import Marvel.Stats
import Marvel.Hercules.Helpers
import Marvel.GameValue
import Marvel.Hand

wm58001a :: HeroCard WmHcHero
wm58001a = hero WmHcHero WM.wm58001a (HP $ Static 12) (HandSize 5) (Thw 2) (Atk 1) (Def 2)

hc59001a :: HeroCard WmHcHero
hc59001a = hero WmHcHero HC.hc59001a (HP $ Static 14) (HandSize 4) (Thw 1) (Atk 3) (Def 2)

newtype WmHcHero = WmHcHero (Attrs Hero)
  deriving anyclass IsHero
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WmHcHero where
  getModifiersFor _ (IdentityRef you) (WmHcHero attrs) | you == heroIdentityId attrs && toCardCode attrs == "58001a" = do
    cards <- ionicCards you
    pure [AttackModifier $ fromIntegral $ length cards]
  getModifiersFor _ (IdentityRef you) (WmHcHero attrs) | you == heroIdentityId attrs && toCardCode attrs == "59001a" = do
    n <- giftCount you
    pure [ResourceCostReductionFor (CardIs HC.hc59009) n, ResourceCostReductionFor (CardIs HC.hc59011) n]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WmHcHero where
  getAbilities a@(WmHcHero attrs) = case toCardCode attrs of
    "58001a" -> [limitedWindowAbility a 1 (W.MakesBasicAttack W.After You) ForcedResponse IsSelf NoCost $ runAbility a 1]
    _ -> [(limitedWindowAbility a 1 (W.CardAddedToVictoryDisplay $ CardWithTrait T.Labor) Response IsSelf NoCost $ runAbility a 1) {abilityLimit = PerPhase 1}]

instance RunMessage WmHcHero where
  runMessage msg h@(WmHcHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      case toCardCode attrs of
        "58001a" -> do
          host <- ionicPhysiologyFor you
          for_ host $ \uid -> do
            cards <- ionicCards you
            traverse_ (discardHeldPlayerCard you uid) cards
        _ -> do
          cardSpecific you "hcGainGift" ()
          ready you
          chooseOne you [labeled "$label.done" (pure ()), labeled "$label.changeToB" (changeToAlterEgo you)]
      pure h
    _ -> WmHcHero <$> runMessage msg attrs
