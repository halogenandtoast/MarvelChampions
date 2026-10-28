-- | The allies of The Mad Titan's Shadow's campaign set: Cosmo, Secure the Landing Pad's
-- other side, and the four Captive allies Open the Dungeons frees.
module Marvel.Ally.Allies.MadTitansShadowCampaign (
  cosmoCampaign,
  CosmoCampaign (..),
  ladySifCaptive,
  LadySifCaptive (..),
  fandral,
  Fandral (..),
  hogun,
  Hogun (..),
  volstagg,
  Volstagg (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers qualified as M
import Marvel.Resource

cosmoCampaign :: AllyCard CosmoCampaign
cosmoCampaign = ally CosmoCampaign Cards.cosmoCampaign (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype CosmoCampaign = CosmoCampaign (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | The first player gains control of Cosmo. Cosmo does not count against the ally
-- limit. Forced Interrupt: when Cosmo leaves play, remove him from the game (he never
-- reaches a discard pile or a hand).
instance HasModifiersFor CosmoCampaign where
  getModifiersFor _ (AllyRef aid) (CosmoCampaign a)
    | aid == allyId a = pure [NotCountedAgainstAllyLimit, ControlledByFirstPlayer, RemovedFromGameInsteadOfLeavingPlay]
  getModifiersFor _ _ _ = pure []

instance RunMessage CosmoCampaign where
  runMessage msg (CosmoCampaign attrs) = CosmoCampaign <$> runMessage msg attrs

ladySifCaptive :: AllyCard LadySifCaptive
ladySifCaptive = ally LadySifCaptive Cards.ladySifCaptive (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype LadySifCaptive = LadySifCaptive (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Action: spend a [physical] resource → ready Lady Sif (only exhausted, so it does
-- something).
instance HasAbilities LadySifCaptive where
  getAbilities a@(LadySifCaptive attrs) =
    [ability a 1 Action (OwnsThis <> AllyExists (M.AllyWithId (allyId attrs) <> M.ExhaustedAlly)) (ResourceCost $ Just Physical) (runAbility a 1)]

instance RunMessage LadySifCaptive where
  runMessage msg a@(LadySifCaptive attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> a <$ ready (allyId attrs)
    _ -> LadySifCaptive <$> runMessage msg attrs

fandral :: AllyCard Fandral
fandral = ally Fandral Cards.fandral (Thw 3, 2) (Atk 1, 1) (HP 3)

newtype Fandral = Fandral (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | [star] When Fandral uses his basic THW, ignore any crisis icons in play.
instance HasModifiersFor Fandral where
  getModifiersFor _ (AllyRef aid) (Fandral a)
    | aid == allyId a = pure [BasicThwartIgnoresCrisis]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fandral where
  runMessage msg (Fandral attrs) = Fandral <$> runMessage msg attrs

-- | [star] Hogun's attacks gain piercing: the keyword on his card.
hogun :: AllyCard Hogun
hogun = ally Hogun Cards.hogun (Thw 1, 1) (Atk 3, 2) (HP 3)

newtype Hogun = Hogun (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Hogun where
  runMessage msg (Hogun attrs) = Hogun <$> runMessage msg attrs

-- | Retaliate 1 and Toughness: his keywords.
volstagg :: AllyCard Volstagg
volstagg = ally Volstagg Cards.volstagg (Thw 1, 1) (Atk 1, 1) (HP 5)

newtype Volstagg = Volstagg (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage Volstagg where
  runMessage msg (Volstagg attrs) = Volstagg <$> runMessage msg attrs
