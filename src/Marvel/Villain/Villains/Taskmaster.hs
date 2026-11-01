module Marvel.Villain.Villains.Taskmaster where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Boost ()
import Marvel.Card ()
import Marvel.Cost
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

newtype Taskmaster = Taskmaster (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

taskmasterVillain :: VillainCard Taskmaster
taskmasterVillain = taskmasterVillain1

taskmasterVillain1 :: VillainCard Taskmaster
taskmasterVillain1 = villain Taskmaster Cards.taskmasterVillain1 (Sch 1) (Atk 2) (HP $ PerPlayer 13)

taskmasterVillain2 :: VillainCard Taskmaster
taskmasterVillain2 =
  villainWith Taskmaster Cards.taskmasterVillain2 (Sch 2) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 2)

taskmasterVillain3 :: VillainCard Taskmaster
taskmasterVillain3 =
  villainWith Taskmaster Cards.taskmasterVillain3 (Sch 3) (Atk 3) (HP $ PerPlayer 17) (stageL .~ 3)

-- | Every stage: "Forced Response: After a player changes to hero form, they discard the
-- top card of the encounter deck and take damage equal to the number of boost icons on
-- that card." II and III: "When Revealed: Deal each player an encounter card."
instance HasAbilities Taskmaster where
  getAbilities (Taskmaster a) =
    windowAbility a 1 (IdentityChangedToForm After HeroIdentity) ForcedResponse NoCost (RunAbility (toTarget a) 1)
      : [ windowAbility
            a
            2
            (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain)
            WhenRevealed
            NoCost
            (RunAbility (toTarget a) 2)
        | villainStage a >= 2
        ]

instance RunMessage Taskmaster where
  runMessage msg e@(Taskmaster attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo taskmasterVillain2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo taskmasterVillain3 attrs
        _ -> Taskmaster <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [p | W.IdentityChangesForm p <- windows] \p -> do
        -- the top card is read before it is discarded: nothing comes between
        key <- getDeckKeyFor p
        Pile deck _ <- getPile key
        for_ (take 1 deck) \top -> do
          discardTopOfEncounterDeck 1
          countBoostIcons attrs p [top]
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success p) (sum . map snd -> n) -> do
      when (n > 0) $ damageIdentity p attrs (toDamage n FromAbility)
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      getPlayers >>= traverse_ dealEncounterCard
      pure e
    _ -> Taskmaster <$> runMessage msg attrs
