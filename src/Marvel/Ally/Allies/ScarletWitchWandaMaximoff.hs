module Marvel.Ally.Allies.ScarletWitchWandaMaximoff (
  scarletWitchWandaMaximoff,
  ScarletWitchWandaMaximoff (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card
import Marvel.Entity
import Marvel.Modifier
import Marvel.Ref

scarletWitchWandaMaximoff :: AllyCard ScarletWitchWandaMaximoff
scarletWitchWandaMaximoff =
  ally
    (ScarletWitchWandaMaximoff . (`With` Meta Nothing))
    Cards.scarletWitchWandaMaximoff
    (Thw 1, 1)
    (Atk 1, 1)
    (HP 3)

-- | The use of a basic power her interrupt has answered: which power, and what it gets.
data Use = Use {attacking :: Bool, bonus :: Natural}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Meta = Meta {using :: Maybe Use}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ScarletWitchWandaMaximoff = ScarletWitchWandaMaximoff (Attrs Ally `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly ScarletWitchWandaMaximoff where
  toAllyAttrs (ScarletWitchWandaMaximoff (a `With` _)) = a

instance IsRef ScarletWitchWandaMaximoff where
  toRef = toRef . toAllyAttrs

instance HasAbilities ScarletWitchWandaMaximoff

-- | "+1 to that power for this use", for each boost icon discarded.
instance HasModifiersFor ScarletWitchWandaMaximoff where
  getModifiersFor _ (AllyRef aid) (ScarletWitchWandaMaximoff (a `With` Meta (Just use)))
    | aid == allyId a && bonus use > 0 =
        pure [if attacking use then AttackModifier (bonus use) else ThwartModifier (bonus use)]
  getModifiersFor _ _ _ = pure []

-- | Interrupt: When you use one of Scarlet Witch's basic powers, discard the top card of
-- the encounter deck. For each boost icon discarded this way, Scarlet Witch gets +1 to
-- that power for this use. Optional; a stunned attack or confused thwart is no use (as
-- the Quicksilver FAQ). The power waits while it is answered, then is used again with
-- what it got, which lasts for that use only.
instance RunMessage ScarletWitchWandaMaximoff where
  runMessage msg w@(ScarletWitchWandaMaximoff (attrs `With` meta)) = runQueueT $ case msg of
    AllyMessage aid power | aid == allyId attrs, Just isAttack <- basicPower power -> case using meta of
      Nothing
        | (if isAttack then allyStunned else allyConfused) attrs -> fallThrough
        | otherwise -> do
            chooseOne (allyController attrs)
              [ labeled "$label.scarletWitchDiscardTop" (cardSpecific w "discard" isAttack)
              , labeled "$label.pass" (cardSpecific w "power" (Use isAttack 0))
              ]
            pure w
      -- the power is used with what it got, then it is gone
      Just _ -> ScarletWitchWandaMaximoff . (`With` Meta Nothing) <$> runMessage msg attrs
    CardSpecific (isTarget w -> True) "discard" (fromJSON -> Success (isAttack :: Bool)) -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons w isAttack)
      pure w
    WithBoostIcons (isTarget w -> True) (fromJSON -> Success (isAttack :: Bool)) (sum . map snd -> n) -> do
      cardSpecific w "power" (Use isAttack n)
      pure w
    CardSpecific (isTarget w -> True) "power" (fromJSON -> Success use) -> do
      allyStep (allyId attrs) (if attacking use then AllyAttacked else AllyThwarted)
      pure . ScarletWitchWandaMaximoff $ attrs `With` Meta (Just use)
    _ -> fallThrough
   where
    fallThrough = ScarletWitchWandaMaximoff . (`With` meta) <$> runMessage msg attrs
    basicPower = \case
      AllyAttacked -> Just True
      AllyThwarted -> Just False
      _ -> Nothing
