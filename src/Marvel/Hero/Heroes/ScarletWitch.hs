module Marvel.Hero.Heroes.ScarletWitch (
  scarletWitch,
  ScarletWitch (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source (getBoostIcons, getEncounterDeck)
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

scarletWitch :: HeroCard ScarletWitch
scarletWitch =
  hero
    ScarletWitch
    Cards.scarletWitch
    (HP $ Static 10)
    (HandSize 5)
    (Thw 2)
    (Atk 1)
    (Def 2)

newtype ScarletWitch = ScarletWitch (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Chaos Control — Interrupt: When boost icons on an encounter card would be counted,
-- discard the top card of the encounter deck and count the number of boost icons on
-- that card instead. (Limit once per phase.)
instance HasAbilities ScarletWitch where
  getAbilities (ScarletWitch a) =
    [ label "Chaos Control" $
        limited (PerPhase 1) $
          limitedWindowAbility a 1 W.BoostIconsCounted Interrupt IsSelf NoCost $ runAbility a 1
    ]

-- | The count becomes the discarded card's icons, replacing what it was, a Crest's
-- change to it included (the owner's ruling: the last one used wins). The deck is never
-- empty (it is made again from the discard pile, or the game is lost); ponytail: the
-- empty case counts 0 only to be total.
instance RunMessage ScarletWitch where
  runMessage msg h@(ScarletWitch attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [W.CountingBoostIcons card] _ -> do
      getEncounterDeck >>= \case
        [] -> changeBoostIconCount card (const 0)
        top : _ -> do
          n <- getBoostIcons top
          discardTopOfEncounterDeck 1
          changeBoostIconCount card (const n)
      pure h
    _ -> ScarletWitch <$> runMessage msg attrs
