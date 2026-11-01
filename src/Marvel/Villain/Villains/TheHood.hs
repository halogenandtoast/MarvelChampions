module Marvel.Villain.Villains.TheHood where

import Marvel.Ability
import Marvel.Card ()
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Prelude
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

newtype TheHood = TheHood (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

theHood1, theHood2, theHood3 :: VillainCard TheHood
theHood1 = villain TheHood Cards.theHood1 (Sch 1) (Atk 1) (HP $ PerPlayer 14)
theHood2 = villainWith TheHood Cards.theHood2 (Sch 2) (Atk 1) (HP $ PerPlayer 16) (stageL .~ 2)
theHood3 = villainWith TheHood Cards.theHood3 (Sch 3) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 3)

instance HasAbilities TheHood where
  getAbilities (TheHood a) =
    [ windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost $ runAbility a 1
    | villainStage a >= 2
    ]

-- | Foul Play is always queued: its discard must see the deck after any earlier work.
foulPlay :: (ReverseQueue m) => IdentityId -> m ()
foulPlay you = cardSpecific GameRef "hoodFoulPlay" (you, Nothing :: Maybe MainSchemeId)

recruitModular :: (ReverseQueue m) => m ()
recruitModular = cardSpecific GameRef "hoodRecruit" ()

instance RunMessage TheHood where
  runMessage msg e@(TheHood attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (Normal, 1) -> advanceVillain ident >> pure (advanceVillainTo theHood2 attrs)
        (Expert, 2) -> advanceVillain ident >> pure (advanceVillainTo theHood3 attrs)
        _ -> TheHood <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ recruitModular
    _ -> TheHood <$> runMessage msg attrs
