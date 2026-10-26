module Marvel.Villain.Villains.Ultron where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue.Types
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (Choice (RunAbility))
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Stats
import Marvel.Trait
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window hiding (VillainDamaged)

newtype Ultron = Ultron (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ultron :: VillainCard Ultron
ultron = ultron1

ultron1 :: VillainCard Ultron
ultron1 = villain Ultron Cards.ultron1 (Sch 1) (Atk 2) (HP $ PerPlayer 17)

ultron2 :: VillainCard Ultron
ultron2 = villainWith Ultron Cards.ultron2 (Sch 2) (Atk 2) (HP $ PerPlayer 22) (stageL .~ 2)

ultron3 :: VillainCard Ultron
ultron3 = villainWith Ultron Cards.ultron3 (Sch 2) (Atk 4) (HP $ PerPlayer 27) (stageL .~ 3)

-- | Ultron (III): each Drone minion gets +1 ATK and +1 hit point.
instance HasModifiersFor Ultron where
  getModifiersFor _ (MinionRef mid) (Ultron a) | villainStage a == 3 = do
    isDrone <- minionMatches (MinionWithTrait Drone) mid
    pure $ if isDrone then [AttackModifier 1, HitPointModifier 1] else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities Ultron where
  getAbilities (Ultron a) = case villainStage a of
    1 ->
      [ windowAbility a 1 (EnemyAttacked After self You) ForcedResponse NoCost $
          RunAbility (toTarget a) 1
      ]
    2 ->
      [ windowAbility a 1 (EnemyWouldAttack self You) ForcedInterrupt NoCost $
          RunAbility (toTarget a) 1
      ]
    3 ->
      [ windowAbility
          a
          1
          (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain)
          WhenRevealed
          NoCost
          $ RunAbility (toTarget a) 1
      ]
    _ -> error "Invalid stage"
   where
    self = EnemyWithId $ EnemyVillainId $ villainId a

instance RunMessage Ultron where
  runMessage msg e@(Ultron attrs) = runQueueT $ case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainDefeated -> do
        difficulty <- getDifficulty
        case (difficulty, villainStage attrs) of
          (_, 1) -> do
            advanceVillain ident
            pure $ advanceVillainTo ultron2 attrs
          (Normal, 2) -> Ultron <$> runMessage msg attrs
          (Expert, 2) -> do
            advanceVillain ident
            pure $ advanceVillainTo ultron3 attrs
          (_, 3) -> Ultron <$> runMessage msg attrs
          (_, _) -> error "Invalid ultron progression"
      -- Ultron (II) gets +1 ATK for each Drone minion engaged with the player attacked
      VillainBeginAttack target | villainStage attrs == 2 -> do
        drones <- selectListCount $ MinionWithTrait Drone <> MinionEngagedWith (IdentityWithId target)
        Ultron attrs' <- Ultron <$> runMessage msg attrs
        pure . Ultron $ attrs' & attackingL . _Just . attackDamageL +~ drones
      -- Ultron (III) cannot take damage while a Drone minion is in play
      VillainDamaged _ _ | villainStage attrs == 3 -> do
        drones <- selectAny (MinionWithTrait Drone)
        if drones then pure e else Ultron <$> runMessage msg attrs
      _ -> Ultron <$> runMessage msg attrs
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      case villainStage attrs of
        1 -> do
          mainScheme <- selectJust MainScheme
          chooseOne
            ident
            [ labeled (countLabel "placeThreatOnMainScheme" 1) $ placeThreat mainScheme 1
            , labeled "$label.topCardAsDrone" $ putTopOfDeckIntoPlayAsDrone ident
            ]
        2 -> putTopOfDeckIntoPlayAsDrone ident
        3 -> searchForAndRevealScheme Cards.ultronsImperative
        _ -> error "Invalid choice"
      pure e
    _ -> Ultron <$> runMessage msg attrs
