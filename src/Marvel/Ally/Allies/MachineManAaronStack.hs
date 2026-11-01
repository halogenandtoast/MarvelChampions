module Marvel.Ally.Allies.MachineManAaronStack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ability qualified as Ability ()
import Marvel.Card ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Query ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Upgrade.Cards qualified as Upgrades ()
import Marvel.Window qualified as W
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Payment ()
machineManAaronStack :: AllyCard MachineManAaronStack
machineManAaronStack = ally (MachineManAaronStack . (`With` Meta 0)) Cards.machineManAaronStack (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Meta = Meta {bonusForUse :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MachineManAaronStack = MachineManAaronStack (Attrs Ally `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly MachineManAaronStack where
  toAllyAttrs (MachineManAaronStack (attrs `With` _)) = attrs

instance IsRef MachineManAaronStack where
  toRef = toRef . toAllyAttrs

instance HasModifiersFor MachineManAaronStack where
  getModifiersFor _ (AllyRef aid) (MachineManAaronStack (a `With` meta)) | aid == allyId a = pure [ThwartModifier (bonusForUse meta), AttackModifier (bonusForUse meta)]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MachineManAaronStack where
  getAbilities (MachineManAaronStack (a `With` _)) =
    [ limitedWindowAbility a 1 (W.WindowOneOf [W.AllyAttacked W.When (AllyWithId $ allyId a) AnyEnemy, W.AllyThwarted W.When (AllyWithId $ allyId a) AnyScheme])
        Interrupt OwnsThis (UpToResourcesCost 3) (runAbility a 1)
    ]

instance RunMessage MachineManAaronStack where
  runMessage msg (MachineManAaronStack (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      let bonus = sum [n | XPayment n <- payments payment]
      addToAllyBasicPower (allyId attrs) bonus
      pure $ MachineManAaronStack (attrs `With` Meta bonus)
    CheckWindows ws | any endsUse ws -> pure $ MachineManAaronStack (attrs `With` Meta 0)
    _ -> MachineManAaronStack . (`With` meta) <$> runMessage msg attrs
   where
    endsUse (W.Window W.After (W.AllyAttack aid _)) = aid == allyId attrs
    endsUse (W.Window W.After (W.AllyThwart aid _)) = aid == allyId attrs
    endsUse (W.Window W.After (W.FriendlyCharacterAttack (AllyCharacter aid) _)) = aid == allyId attrs
    endsUse _ = False
