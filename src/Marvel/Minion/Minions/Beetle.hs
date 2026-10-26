module Marvel.Minion.Minions.Beetle (
  beetle,
  Beetle (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Resource
import Marvel.Window qualified as W

beetle :: MinionCard Beetle
beetle = minion Beetle Cards.beetle (Sch 1) (Atk 1) (HP 4)

newtype Beetle = Beetle (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Interrupt: When Beetle is defeated, choose to either spend a [physical]
-- resource or shuffle Beetle into the encounter deck.
instance HasAbilities Beetle where
  getAbilities (Beetle a) =
    [ windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId a) ForcedInterrupt NoCost $
        RunAbility (toTarget a) 1
    ]

-- | The player who defeated him chooses, and pays; with no [physical] resource to spend
-- he is shuffled in (owner ruling). Shuffled in, he is not defeated: not discarded, and
-- what is attached to him is discarded as he leaves play.
instance RunMessage Beetle where
  runMessage msg e@(Beetle attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 windows _ -> do
      defeater <- case [s | W.DefeatedMinion _ _ s _ <- windows] of
        source : _ -> getSourceController source
        [] -> pure Nothing
      let you = fromMaybe ident defeater
          cost = MultiResourceCost [Just Physical]
      canPay <- canAffordCost you (toSource attrs) cost
      chooseOrRunOne you $
        labeled "$label.shuffleBeetleIntoEncounterDeck" (cardSpecific attrs "shuffle" ())
          : [labeled "$label.spendAPhysicalResource" (payForTreachery you cost) | canPay]
      pure e
    CardSpecific (isTarget attrs -> True) "shuffle" _ -> do
      cancelMinionDefeat (minionId attrs)
      removeFromGame attrs
      shuffleIntoEncounterDeck [MkEncounterCard (CardId $ unMinionId $ minionId attrs) (getCardDef attrs)]
      pure e
    _ -> Beetle <$> runMessage msg attrs
