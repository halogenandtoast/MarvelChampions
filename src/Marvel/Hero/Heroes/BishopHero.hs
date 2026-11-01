module Marvel.Hero.Heroes.BishopHero where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

bishopHero :: HeroCard BishopHero
bishopHero = hero BishopHero Cards.bishopHero (HP $ Static 12) (HandSize 5) (Thw 2) (Atk 2) (Def 1)

newtype BishopHero = BishopHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BishopHero where
  getAbilities a@(BishopHero attrs) =
    [label "$label.energyAbsorption" $ limitedWindowAbility a 1
      (W.IdentityDamagedByAttack W.After $ IdentityWithId $ heroIdentityId attrs)
      Response IsSelf NoCost (runAbility a 1)]

instance RunMessage BishopHero where
  runMessage msg a@(BishopHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [damageAmount damage | W.IdentityTookDamage ident _ damage <- windows, ident == you] $ \n -> discardFromDeck you n attrs
      pure a
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      for_ [card | PlayerCard card <- cards, cardMatch (CardWithType ResourceType) card] $ addToHand (heroIdentityId attrs)
      pure a
    _ -> BishopHero <$> runMessage msg attrs
