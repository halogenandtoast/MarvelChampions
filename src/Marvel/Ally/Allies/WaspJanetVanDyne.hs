module Marvel.Ally.Allies.WaspJanetVanDyne (
  waspJanetVanDyne,
  WaspJanetVanDyne (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def ()
import Marvel.Game.Source (getModifiers)
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Ref ()
import Marvel.Resource

waspJanetVanDyne :: AllyCard WaspJanetVanDyne
waspJanetVanDyne = ally WaspJanetVanDyne Cards.waspJanetVanDyne (Thw 1, 1) (Atk 3, 1) (HP 0)

newtype WaspJanetVanDyne = WaspJanetVanDyne (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Wasp gets +1 hit point for each pym counter on her.
instance HasModifiersFor WaspJanetVanDyne where
  getModifiersFor _ (AllyRef aid) (WaspJanetVanDyne a)
    | aid == allyId a = pure [HitPointModifier (allyCounters a)]
  getModifiersFor _ _ _ = pure []

-- | Interrupt: When Wasp enters play, place 1 pym counter on her (to a maximum of 3) for
-- each [energy] resource you overpaid for Wasp's cost. What was paid beyond her cost is
-- overpaid, the [energy] resources (a wild one declared as one) taken as overpaid before
-- the others. Put into play, nothing is paid; with no counters she has no hit points
-- and is defeated (RR "Hit Points").
instance RunMessage WaspJanetVanDyne where
  runMessage msg (WaspJanetVanDyne attrs) = runQueueT $ case msg of
    AllyMessage aid (AllyPaidWith payment) | aid == allyId attrs -> do
      resources <- paymentResources payment
      cost <- costOf
      let energy = length $ filter (== Energy) resources
          overpaid = min 3 . fromIntegral . max 0 $ min energy (length resources - cost)
      when (overpaid == 0) $ defeatAlly aid
      WaspJanetVanDyne . (\a -> a {allyCounters = overpaid}) <$> runMessage msg attrs
    _ -> WaspJanetVanDyne <$> runMessage msg attrs
   where
    -- ponytail: her printed cost is 0, so only a cost increase matters, read from her
    -- controller's modifiers as she enters play (a one-off reduction already spent can
    -- only take it lower, and 0 is the floor)
    costOf = do
      modifiers <- getModifiers (allyController attrs)
      pure $ max 0 $ sum [fromIntegral n | AdditionalResourceCost matcher n <- modifiers, cardMatch matcher attrs]
        - sum [fromIntegral n | ResourceCostReduction n <- modifiers]
        - sum [fromIntegral n | ResourceCostReductionFor matcher n <- modifiers, cardMatch matcher attrs]
