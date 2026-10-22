-- | The Rise of Red Skull's Hydra Campaign set: four Tech upgrades used once, and four
-- double-sided Condition upgrades, Basic and Improved. All are Setup, in play from the
-- start of each game ('Marvel.Identity' puts them there); the Conditions are Permanent.
module Marvel.Upgrade.Upgrades.HydraCampaign (
  adrenalStims,
  AdrenalStims (..),
  tacticalScanner,
  TacticalScanner (..),
  emergencyTeleporter,
  EmergencyTeleporter (..),
  laserCannon,
  LaserCannon (..),
  basicThwartUpgrade,
  improvedThwartUpgrade,
  ThwartUpgrade (..),
  basicAttackUpgrade,
  improvedAttackUpgrade,
  AttackUpgrade (..),
  basicDefenseUpgrade,
  improvedDefenseUpgrade,
  DefenseUpgrade (..),
  basicRecoveryUpgrade,
  improvedRecoveryUpgrade,
  RecoveryUpgrade (..),
) where

import Marvel.Prelude

import Data.Text qualified as T

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Discard
import Marvel.Identity.Types
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window (WindowMatcher (..), WindowTiming (After))

-- * Tech upgrades: "Hero Action: Discard this card and remove it from the campaign log → …"

techAction :: Attrs Upgrade -> [Ability]
techAction a = [ability a 1 HeroAction OwnsThis (DiscardCost $ toTarget a) (runAbility a 1)]

-- | The effect, once the card is discarded: crossed out of the log, so it is not in the
-- deck again.
techUsed :: (ReverseQueue m) => Attrs Upgrade -> m () -> m ()
techUsed a effect = do
  effect
  removeFromCampaignLog (upgradeController a) (toCardCode a)

adrenalStims :: UpgradeCard AdrenalStims
adrenalStims = upgrade AdrenalStims Cards.adrenalStims

newtype AdrenalStims = AdrenalStims (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AdrenalStims where
  getAbilities (AdrenalStims a) = techAction a

-- | Ready your hero and heal 5 damage from them.
instance RunMessage AdrenalStims where
  runMessage msg u@(AdrenalStims a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let you = upgradeController a
      techUsed a (ready you >> heal you 5)
      pure u
    _ -> AdrenalStims <$> runMessage msg a

tacticalScanner :: UpgradeCard TacticalScanner
tacticalScanner = upgrade TacticalScanner Cards.tacticalScanner

newtype TacticalScanner = TacticalScanner (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TacticalScanner where
  getAbilities (TacticalScanner a) = techAction a

-- | Draw 5 cards.
instance RunMessage TacticalScanner where
  runMessage msg u@(TacticalScanner a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      techUsed a (drawCards (upgradeController a) 5)
      pure u
    _ -> TacticalScanner <$> runMessage msg a

emergencyTeleporter :: UpgradeCard EmergencyTeleporter
emergencyTeleporter = upgrade EmergencyTeleporter Cards.emergencyTeleporter

newtype EmergencyTeleporter = EmergencyTeleporter (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EmergencyTeleporter where
  getAbilities (EmergencyTeleporter a) = techAction a

-- | Search your deck and discard pile for an ally, put it into play, and give it a
-- tough status card.
instance RunMessage EmergencyTeleporter where
  runMessage msg u@(EmergencyTeleporter a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let you = upgradeController a
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      let allies = filter (cardMatch $ CardWithType AllyType) (deck <> discard)
      techUsed a do
        unless (null allies) $
          focusing (map PlayerCard allies) $
            chooseOne you
              [ targeting card do
                  putIntoPlay you card
                  giveStatus (AllyId $ unCardId $ pcCardId card) ToughStatus
              | card <- allies
              ]
        shuffleDeck you
      pure u
    _ -> EmergencyTeleporter <$> runMessage msg a

laserCannon :: UpgradeCard LaserCannon
laserCannon = upgrade LaserCannon Cards.laserCannon

newtype LaserCannon = LaserCannon (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LaserCannon where
  getAbilities (LaserCannon a) = techAction a

-- | Deal 5 damage to the villain and each enemy engaged with you.
instance RunMessage LaserCannon where
  runMessage msg u@(LaserCannon a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      let you = upgradeController a
      villains <- selectList VillainEnemy
      minions <- selectList $ MinionEngagedWith (IdentityWithId you)
      techUsed a $ dealDamageToEach (villains <> map EnemyMinionId minions) a (toDamage 5 FromAbility)
      pure u
    _ -> LaserCannon <$> runMessage msg a

-- * Condition upgrades: "You get +X hit points. Your hero gets +1 …", the Improved side
-- with a response to draw a card

-- | The modifiers a Condition upgrade gives its controller.
conditionModifiers :: (Applicative m) => [Modifier] -> Target -> Attrs Upgrade -> m [Modifier]
conditionModifiers ms (IdentityRef iid) a | iid == upgradeController a = pure ms
conditionModifiers _ _ _ = pure []

-- | The Improved side's "exhaust this card → draw 1 card", in the window given.
improvedDraw :: Attrs Upgrade -> AbilityType -> WindowMatcher -> [Ability]
improvedDraw a aType window
  | isImproved a = [windowAbility a 1 window aType ExhaustCost (runAbility a 1)]
  | otherwise = []

-- | The "b" face.
isImproved :: Attrs Upgrade -> Bool
isImproved a = "b" `T.isSuffixOf` unCardCode (toCardCode a)

basicThwartUpgrade :: UpgradeCard ThwartUpgrade
basicThwartUpgrade = upgrade ThwartUpgrade Cards.basicThwartUpgrade

improvedThwartUpgrade :: UpgradeCard ThwartUpgrade
improvedThwartUpgrade = upgrade ThwartUpgrade Cards.improvedThwartUpgrade

newtype ThwartUpgrade = ThwartUpgrade (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ThwartUpgrade where
  getModifiersFor _ target (ThwartUpgrade a) = conditionModifiers [HitPointModifier 2, ThwartModifier 1] target a

-- | Response: after you defeat a side scheme.
instance HasAbilities ThwartUpgrade where
  getAbilities (ThwartUpgrade a) = improvedDraw a Response (DefeatsSideScheme You)

instance RunMessage ThwartUpgrade where
  runMessage msg u@(ThwartUpgrade a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> u <$ drawCards (upgradeController a) 1
    _ -> ThwartUpgrade <$> runMessage msg a

basicAttackUpgrade :: UpgradeCard AttackUpgrade
basicAttackUpgrade = upgrade AttackUpgrade Cards.basicAttackUpgrade

improvedAttackUpgrade :: UpgradeCard AttackUpgrade
improvedAttackUpgrade = upgrade AttackUpgrade Cards.improvedAttackUpgrade

newtype AttackUpgrade = AttackUpgrade (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor AttackUpgrade where
  getModifiersFor _ target (AttackUpgrade a) = conditionModifiers [HitPointModifier 1, AttackModifier 1] target a

-- | Hero Response: after you defeat a minion. "You" is the identity (RR, You, Your: "after
-- you attack and defeat a minion" does not trigger "when an ally under that player's control
-- attacks and defeats a minion"), so an ally's defeat of a minion does not count.
instance HasAbilities AttackUpgrade where
  getAbilities (AttackUpgrade a) = improvedDraw a HeroResponse (EnemyDefeated After MinionEnemy (DamageByPlayer You))

instance RunMessage AttackUpgrade where
  runMessage msg u@(AttackUpgrade a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> u <$ drawCards (upgradeController a) 1
    _ -> AttackUpgrade <$> runMessage msg a

basicDefenseUpgrade :: UpgradeCard DefenseUpgrade
basicDefenseUpgrade = upgrade DefenseUpgrade Cards.basicDefenseUpgrade

improvedDefenseUpgrade :: UpgradeCard DefenseUpgrade
improvedDefenseUpgrade = upgrade DefenseUpgrade Cards.improvedDefenseUpgrade

newtype DefenseUpgrade = DefenseUpgrade (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor DefenseUpgrade where
  getModifiersFor _ target (DefenseUpgrade a) = conditionModifiers [HitPointModifier 3, DefenseModifier 1] target a

-- | Hero Response: after you defend against an attack.
instance HasAbilities DefenseUpgrade where
  getAbilities (DefenseUpgrade a) = improvedDraw a HeroResponse (HeroDefended After You AnyEnemy)

instance RunMessage DefenseUpgrade where
  runMessage msg u@(DefenseUpgrade a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> u <$ drawCards (upgradeController a) 1
    _ -> DefenseUpgrade <$> runMessage msg a

basicRecoveryUpgrade :: UpgradeCard RecoveryUpgrade
basicRecoveryUpgrade = upgrade RecoveryUpgrade Cards.basicRecoveryUpgrade

improvedRecoveryUpgrade :: UpgradeCard RecoveryUpgrade
improvedRecoveryUpgrade = upgrade RecoveryUpgrade Cards.improvedRecoveryUpgrade

newtype RecoveryUpgrade = RecoveryUpgrade (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RecoveryUpgrade where
  getModifiersFor _ target (RecoveryUpgrade a) = conditionModifiers [HitPointModifier 4, RecoveryModifier 1] target a

-- | Response: after you use your REC.
instance HasAbilities RecoveryUpgrade where
  getAbilities (RecoveryUpgrade a) = improvedDraw a Response (UsesRecovery You)

instance RunMessage RecoveryUpgrade where
  runMessage msg u@(RecoveryUpgrade a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> u <$ drawCards (upgradeController a) 1
    _ -> RecoveryUpgrade <$> runMessage msg a
