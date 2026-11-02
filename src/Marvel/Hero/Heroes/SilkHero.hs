module Marvel.Hero.Heroes.SilkHero where
import Marvel.Prelude
import Data.List (nub)
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats

silkHero :: HeroCard SilkHero
silkHero = hero SilkHero Cards.silkHero (HP $ Static 10) (HandSize 5) (Thw 1) (Atk 2) (Def 3)
newtype SilkHero = SilkHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage SilkHero where
  runMessage msg u@(SilkHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      discard <- getEncounterDiscard
      let ids = [CardId (unMinionId mid) | W.DefeatedMinion mid _ _ _ <- windows] <> [CardId (unSideSchemeId sid) | W.DefeatedSideSchemeBy sid _ _ <- windows]
          cards = [card | card <- discard, ecCardId card `elem` ids] <> [card | W.ResolvedTreacheryCard _ card <- windows, card `elem` discard]
      chooseOne you [targeting card (tuckEncounterCard you card) | card <- nub cards]
      pure u
    _ -> SilkHero <$> runMessage msg attrs

instance HasAbilities SilkHero where
  getAbilities u@(SilkHero _attrs) = [limitedWindowAbility u 1 (W.WindowOneOf [W.MinionDefeatedBy W.After AnyMinion (SourceIsHero You), W.SideSchemeDefeatedBy AnySideScheme (SourceIsHero You), W.TreacheryResolvedBy You]) Response NoCriteria NoCost (runAbility u 1)]
