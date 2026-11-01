module Marvel.Hero.Heroes.Cable where

import Marvel.Prelude
import Data.List ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Status ()
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword ()
import {-# SOURCE #-} Marvel.Game ()

import Marvel.Question ( runAbility )
import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Types
import Marvel.Stats
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Runner ()

cable40001a :: HeroCard Cable40001A
cable40001a = hero Cable40001A Cards.cable40001a (HP $ Static 12) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype Cable40001A = Cable40001A (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40001A where
  runMessage msg a@(Cable40001A attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ ready (heroIdentityId attrs)
    _ -> Cable40001A <$> runMessage msg attrs

instance HasAbilities Cable40001A where
  getAbilities (Cable40001A attrs) =
    [limited (PerPhase 1) $ limitedWindowAbility attrs 1
      (W.SideSchemeDefeatedBy AnySideScheme (SourceIsHero $ IdentityWithId $ heroIdentityId attrs))
      Response (IsSelf <> Exhausted) NoCost (runAbility attrs 1)]

instance HasModifiersFor Cable40001A where
  getModifiersFor _ (IdentityRef you) (Cable40001A attrs) | you == heroIdentityId attrs = do
    purged <- any (\c -> toCardCode c == "40006" && pcOwner c == Just you) <$> getPlayerVictoryDisplay
    pure $ if purged then [TraitModifier Trait.Psionic, ThwartModifier 1, AttackModifier 1, DefenseModifier 1] else []
  getModifiersFor _ _ _ = pure []
