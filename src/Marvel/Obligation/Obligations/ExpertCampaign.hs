-- | The Rise of Red Skull's Expert Campaign Set: obligations a player adds to their own
-- deck to heal. Drawn, one goes into play in their play area ('PutObligationIntoPlay')
-- and stays there, its Alter-Ego Action theirs alone to discard it with.
module Marvel.Obligation.Obligations.ExpertCampaign (
  zolasAlgorithm,
  ZolasAlgorithm (..),
  medicalEmergency,
  MedicalEmergency (..),
  martialLaw,
  MartialLaw (..),
  antiHeroPropaganda,
  AntiHeroPropaganda (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Question
import Marvel.Ref
import Marvel.Entity
import Marvel.Resource
import Marvel.Window (WindowMatcher (TurnEnds), WindowTiming (When))

-- | "Alter-Ego Action: … → discard this card", what the cost leaves to do done first.
discardAction :: Attrs Obligation -> Cost -> [Ability]
discardAction a cost = [ability a 1 AlterEgoAction NoCriteria cost (runAbility a 1)]

-- | Whose play area it is in.
owner :: Attrs Obligation -> Maybe IdentityId
owner = obligationOwner

ownerModifiers :: (Applicative m) => [Modifier] -> Target -> Attrs Obligation -> m [Modifier]
ownerModifiers ms (IdentityRef iid) a | Just iid == owner a = pure ms
ownerModifiers _ _ _ = pure []

zolasAlgorithm :: ObligationCard ZolasAlgorithm
zolasAlgorithm = obligation ZolasAlgorithm Cards.zolasAlgorithm

newtype ZolasAlgorithm = ZolasAlgorithm (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Exhaust your alter-ego and spend a [mental] resource → discard this card.
instance HasAbilities ZolasAlgorithm where
  getAbilities (ZolasAlgorithm a) = discardAction a (ExhaustCost <> ResourceCost (Just Mental))

instance RunMessage ZolasAlgorithm where
  runMessage msg o@(ZolasAlgorithm attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromPlay attrs
    _ -> ZolasAlgorithm <$> runMessage msg attrs

medicalEmergency :: ObligationCard MedicalEmergency
medicalEmergency = obligation MedicalEmergency Cards.medicalEmergency

newtype MedicalEmergency = MedicalEmergency (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Forced Response: at the end of your turn, take 1 damage if you are in hero form.
-- Alter-Ego Action: discard the top 5 cards of your deck and spend a [physical]
-- resource → discard this card.
instance HasAbilities MedicalEmergency where
  getAbilities (MedicalEmergency a) =
    discardAction a (ResourceCost (Just Physical))
      <> [ windowAbility a 2 (TurnEnds When (IdentityWithId you <> HeroIdentity)) ForcedResponse NoCost (runAbility a 2)
         | Just you <- [owner a]
         ]

instance RunMessage MedicalEmergency where
  runMessage msg o@(MedicalEmergency attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (owner attrs) (`discardTopOfDeck` 5)
      o <$ removeFromPlay attrs
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      for_ (owner attrs) \you -> damageIdentity you attrs (toDamage 1 FromAbility)
      pure o
    _ -> MedicalEmergency <$> runMessage msg attrs

martialLaw :: ObligationCard MartialLaw
martialLaw = obligation MartialLaw Cards.martialLaw

newtype MartialLaw = MartialLaw (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Your hand size is reduced by 1. Alter-Ego Action: deal yourself an encounter card
-- and spend a [energy] resource → discard this card.
instance HasAbilities MartialLaw where
  getAbilities (MartialLaw a) = discardAction a (ResourceCost (Just Energy))

instance HasModifiersFor MartialLaw where
  getModifiersFor _ target (MartialLaw a) = ownerModifiers [HandSizePenalty 1] target a

instance RunMessage MartialLaw where
  runMessage msg o@(MartialLaw attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (owner attrs) dealEncounterCard
      o <$ removeFromPlay attrs
    _ -> MartialLaw <$> runMessage msg attrs

antiHeroPropaganda :: ObligationCard AntiHeroPropaganda
antiHeroPropaganda = obligation AntiHeroPropaganda Cards.antiHeroPropaganda

newtype AntiHeroPropaganda = AntiHeroPropaganda (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Your hero gets -1 THW, -1 ATK, and -1 DEF. Alter-Ego Action: take 2 damage and
-- spend a [wild] resource → discard this card.
instance HasAbilities AntiHeroPropaganda where
  getAbilities (AntiHeroPropaganda a) = discardAction a (DamageCost 2 <> ResourceCost (Just Wild))

instance HasModifiersFor AntiHeroPropaganda where
  getModifiersFor _ target (AntiHeroPropaganda a) =
    ownerModifiers [ThwartPenalty 1, AttackPenalty 1, DefensePenalty 1] target a

instance RunMessage AntiHeroPropaganda where
  runMessage msg o@(AntiHeroPropaganda attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromPlay attrs
    _ -> AntiHeroPropaganda <$> runMessage msg attrs
