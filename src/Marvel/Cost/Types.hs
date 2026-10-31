module Marvel.Cost.Types where

import Marvel.Prelude

import Data.List (nub)
import {-# SOURCE #-} Marvel.Card.Def (CardMatcher)
import {-# SOURCE #-} Marvel.Matchers.Types (AllyMatcher, CharacterMatcher, SupportMatcher, UpgradeMatcher)
import Marvel.Id (EnemyId)
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Trait.Types (Trait)

data Cost
  = Costs [Cost]
  | ResourceCost (Maybe Resource)
  | DynamicResourceCost (Maybe Resource)
  | -- | printed X cost: minimum meaningful X, and an effect bound only when its
    -- resource cost is ignored (finite choices without capping ordinary overpayment)
    PrintedXCost Natural UpgradeMatcher
  | -- | spend one through this many resources ("up to" in a cost; Machine Man)
    UpToResourcesCost Natural
  | MultiResourceCost [Maybe Resource]
  | PrintedResourceCost [Resource]
  | TouchedDamageCost Natural
  | DamageCharacterCost CharacterMatcher Natural
  | DamageCost Natural
  | IndirectDamageCost Natural
  | HealCost Natural
  | DamageThisCost Natural
  | PlaceThreatOnMainSchemeCost Natural
  | ExhaustCost
  | -- | at least so many cards from hand, and at most so many (any number for Nothing)
    DiscardHandCardsCost Natural (Maybe Natural)
  | DiscardCost Target
  | DiscardControlledCardCost CardMatcher
  | RemoveFromGameCost Target
  | DiscardHandCardsWithTotalCost CardMatcher Natural
  | ExactCountersCost Natural
  | DiscardToughCost Natural
  | GiveEnemyToughCost EnemyId
  | GiveEnemyBoostCost EnemyId Natural
  | -- | so many cards discarded at random from the payer's hand
    DiscardRandomHandCardsCost Natural
  | -- | discard matching cards at random from the payer's hand
    DiscardRandomHandCardsMatchingCost Natural CardMatcher
  | CountersFromUpgradeCost UpgradeMatcher Natural
  | UseCost
  | NoCost
  | -- | as many resources, each of a different type
    DifferentResourcesCost Natural
  | -- | exhaust an upgrade the payer controls, chosen among those matching
    ExhaustUpgradeCost UpgradeMatcher
  | ExhaustSupportCost SupportMatcher
  | -- | return an upgrade the payer controls, chosen among those matching, to hand
    ReturnUpgradeToHandCost UpgradeMatcher
  | ReturnAllyToHandCost AllyMatcher
  | ExhaustAlliesWithTraitCost Trait Natural (Maybe Natural)
  | -- | discard an upgrade the payer controls, chosen among those matching
    DiscardUpgradeCost UpgradeMatcher
  | -- | discard a controlled ally, retaining its printed card in the payment
    DiscardAllyCost AllyMatcher
  | -- | discard a support the payer controls
    DiscardSupportCost SupportMatcher
  | -- | discard one controlled ally or support, chosen among those matching
    DiscardAllyOrSupportCost AllyMatcher SupportMatcher
  | -- | discard the top card of the payer's deck: not payable with an empty deck
    DiscardTopOfDeckCost
  | -- | discard between so many cards from the top of the payer's deck and so many (as
    -- many as it holds), the payer choosing how many: "discard up to 4 cards from the
    -- top of your deck" (Adam Warlock's Karmic Blast). The cards are paid with.
    DiscardTopOfDeckCardsCost Natural Natural
  | -- | discard as many cards from the top of the payer's deck as the damage they are
    -- about to take: "discard that many cards" (Shield Spell); not payable with fewer
    DiscardTopOfDeckForDamageCost
  | -- | exhaust at least so many allies the payer controls, and at most so many (any
    -- number for Nothing)
    ExhaustAlliesCost Natural (Maybe Natural)
  | -- | exhaust exactly so many allies the payer controls matching (Mass Attack)
    ExhaustAlliesMatchingCost AllyMatcher Natural
  | -- | "discard any number of counters from <this card>": at least so many, the payer
    -- choosing how many, paid as 'CountersPayment' (Power Man)
    CountersFromThisCost Natural
  | -- | the resources of the cost spent only from cards in the payer's hand: "spend a
    -- [physical] resource from your hand" (Blade, Sanctuary)
    ResourcesFromHandCost Cost
  | -- | exhaust a character the payer controls with the trait: their identity or an ally
    ExhaustCharacterWithTraitCost Trait
  | -- | exhaust one controlled character matching, with its identity in the receipt
    ExhaustCharacterCost CharacterMatcher
  | -- | exhaust between the given minimum and maximum number of ready cards the
    -- payer controls with this trait, including identities, allies, supports and upgrades
    ExhaustCardsWithTraitCost Trait Natural Natural
  | -- | discard encounter cards until a minion is discarded, and put it into play
    -- engaged with the payer
    DiscardEncounterCardsUntilMinionCost
  | -- | discard encounter cards until a treachery is discarded, then reveal it;
    -- the reveal must resolve before the effect paid for can begin (Return the Favor)
    RevealEncounterTreacheryCost
  | -- | shuffle this many top encounter cards into the payer's deck as a cost
    ShuffleEncounterCardsIntoOwnDeckCost Natural
  | SearchEncounterSideSchemeCost
  | SearchEncounterMinionCost
  | -- | "deal yourself 1 facedown encounter card"
    DealEncounterCardCost
  | -- | "discard the top card of the encounter deck" (Leading Blow), the card paid with
    DiscardTopEncounterCardCost
  | LookAtTopEncounterCardsAndDiscardOneCost Natural
  | -- | the printed resource cost of the card chosen as the card's target
    TargetPrintedCost
  | -- | choose a hand card as a cost; it remains in hand but cannot pay resources
    ChosenHandCardCost
  | -- | choose a matching character without exhausting or spending that character
    ChooseCharacterWithTraitCost Trait CharacterMatcher
  | -- | nothing more is due, but the payer may go on spending resources: "for each
    -- resource you overpaid" (Ant-Man, Hank Pym)
    MayOverpay
  | -- | so many counters removed from the payer's identity card: "remove 1 growth
    -- counter from him" (Groot)
    IdentityCountersCost Natural
  | PlaceIdentityCountersCost Natural
  | -- | "exhaust the Milano", whoever controls it (The Galaxy's Most Wanted)
    ExhaustMilanoCost
  | -- | the first cost if the payer controls the Milano, the second if not, decided as it is
    -- paid ("… instead if you control the Milano", The Galaxy's Most Wanted)
    IfControlsMilanoCost Cost Cost
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance Monoid Cost where
  mempty = NoCost

instance Semigroup Cost where
  NoCost <> x = x
  x <> NoCost = x
  Costs xs <> Costs ys = Costs $ xs <> ys
  Costs xs <> y = Costs $ xs <> [y]
  x <> Costs ys = Costs $ x : ys
  x <> y = Costs [x, y]

costResources :: Cost -> [Maybe Resource]
costResources NoCost = []
costResources (PlaceThreatOnMainSchemeCost _) = []
costResources ExhaustCost = []
costResources (DiscardCost _) = []
costResources (DiscardControlledCardCost _) = []
costResources (RemoveFromGameCost _) = []
costResources (DiscardHandCardsWithTotalCost _ _) = []
costResources (ExactCountersCost _) = []
costResources (DiscardToughCost _) = []
costResources (GiveEnemyToughCost _) = []
costResources (GiveEnemyBoostCost _ _) = []
costResources (DiscardHandCardsCost _ _) = []
costResources (DiscardRandomHandCardsCost _) = []
costResources (DiscardRandomHandCardsMatchingCost _ _) = []
costResources (ExhaustSupportCost _) = []
costResources (ExhaustUpgradeCost _) = []
costResources (ReturnUpgradeToHandCost _) = []
costResources (ReturnAllyToHandCost _) = []
costResources (ExhaustAlliesWithTraitCost _ _ _) = []
costResources (DiscardUpgradeCost _) = []
costResources (DiscardAllyCost _) = []
costResources (DiscardSupportCost _) = []
costResources (DiscardAllyOrSupportCost _ _) = []
costResources DiscardTopOfDeckCost = []
costResources (DiscardTopOfDeckCardsCost _ _) = []
costResources DiscardTopOfDeckForDamageCost = []
costResources (ExhaustAlliesCost _ _) = []
costResources (ExhaustAlliesMatchingCost _ _) = []
costResources (ResourcesFromHandCost c) = costResources c
costResources (CountersFromThisCost _) = []
costResources (ExhaustCharacterWithTraitCost _) = []
costResources (ExhaustCharacterCost _) = []
costResources (ExhaustCardsWithTraitCost _ _ _) = []
costResources DiscardEncounterCardsUntilMinionCost = []
costResources RevealEncounterTreacheryCost = []
costResources (ShuffleEncounterCardsIntoOwnDeckCost _) = []
costResources SearchEncounterSideSchemeCost = []
costResources SearchEncounterMinionCost = []
costResources DealEncounterCardCost = []
costResources DiscardTopEncounterCardCost = []
costResources (LookAtTopEncounterCardsAndDiscardOneCost _) = []
costResources TargetPrintedCost = []
costResources ChosenHandCardCost = []
costResources (ChooseCharacterWithTraitCost _ _) = []
costResources MayOverpay = []
costResources (PrintedXCost _ _) = []
costResources (UpToResourcesCost _) = [Nothing]
costResources (IdentityCountersCost _) = []
costResources (PlaceIdentityCountersCost _) = []
costResources ExhaustMilanoCost = []
costResources (IfControlsMilanoCost c c') = costResources c <> costResources c'
costResources (DamageCost _) = []
costResources (TouchedDamageCost _) = []
costResources (DamageCharacterCost _ _) = []
costResources (IndirectDamageCost _) = []
costResources (HealCost _) = []
costResources (DamageThisCost _) = []
costResources (CountersFromUpgradeCost _ _) = []
costResources UseCost = []
-- | X is at least 1
costResources (DynamicResourceCost r) = [r]
costResources (ResourceCost r) = [r]
costResources (MultiResourceCost rs) = rs
costResources (PrintedResourceCost rs) = map Just rs
costResources (Costs ps) = concatMap costResources ps
costResources (DifferentResourcesCost n) = replicate (fromIntegral n) Nothing

-- | How many resources of different types the cost asks for.
differentResourcesNeeded :: Cost -> Natural
differentResourcesNeeded = \case
  DifferentResourcesCost n -> n
  Costs cs -> sum (map differentResourcesNeeded cs)
  _ -> 0

-- | Whether the resources can be told as that many of different types: a wild one may
-- be any type, wild among them.
coversDifferentTypes :: Natural -> [Resource] -> Bool
coversDifferentTypes n rs =
  fromIntegral (min 4 (length (nub (filter (/= Wild) rs)) + length (filter (== Wild) rs))) >= n

-- | Icons that must be printed on the cards spent, without wild substitution.
printedResourceRequirements :: Cost -> [Resource]
printedResourceRequirements = \case
  PrintedResourceCost rs -> rs
  Costs cs -> concatMap printedResourceRequirements cs
  ResourcesFromHandCost c -> printedResourceRequirements c
  _ -> []
