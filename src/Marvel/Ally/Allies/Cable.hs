module Marvel.Ally.Allies.Cable where

import Marvel.Prelude
import Data.List ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Status ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword ()
import Marvel.Identity.Types
import Marvel.Projection
import Marvel.Discard

import Marvel.Question ()
import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Stats ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Ally.Runner

cable40014 :: AllyCard Cable40014
cable40014 = ally Cable40014 Cards.cable40014 (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Cable40014 = Cable40014 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40014 where
  runMessage msg a@(Cable40014 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      discardFromDeckUntil you (CardWithType AllyType <> CardMatchesAny [CardWithTrait Trait.XFactor, CardWithTrait Trait.XForce, CardWithTrait Trait.XMen]) attrs
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromDeck (PlayerCard card) -> a <$ addToHand (allyController attrs) card
    _ -> Cable40014 <$> runMessage msg attrs

cable40015 :: AllyCard Cable40015
cable40015 = ally Cable40015 Cards.cable40015 (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Cable40015 = Cable40015 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40015 where
  runMessage msg a@(Cable40015 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardWithTitle "E.V.A.") (SearchTarget $ toRef attrs) ShuffleBackIn
      pure a
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) -> do
      for_ cards (putIntoPlay $ allyController attrs)
      pure a
    _ -> Cable40015 <$> runMessage msg attrs

cable40016 :: AllyCard Cable40016
cable40016 = ally Cable40016 Cards.cable40016 (Thw 1, 1) (Atk 2, 1) (HP 2)

newtype Cable40016 = Cable40016 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40016 where
  runMessage msg a@(Cable40016 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      resources <- paymentResources (allyPaidWith attrs)
      let n = fromIntegral $ length $ filter (== Energy) resources
      players <- getPlayers
      chooseOne you [targeting player $ do
        villains <- selectList AnyVillain
        minions <- selectList (MinionEngagedWith $ IdentityWithId player)
        let enemies = map EnemyVillainId villains <> map EnemyMinionId minions
        dealDamageToEach enemies attrs (toDamage n FromAbility)
        | player <- players]
      pure a
    _ -> Cable40016 <$> runMessage msg attrs

cable40024 :: AllyCard Cable40024
cable40024 = ally Cable40024 Cards.cable40024 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Cable40024 = Cable40024 (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40024 where
  runMessage msg a@(Cable40024 attrs) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "defeatedByConsequential" _ -> do
      heal (allyId attrs) 3
      addAccelerationToken
      pure a
    _ -> Cable40024 <$> runMessage msg attrs

cable40025 :: AllyCard Cable40025
cable40025 = ally Cable40025 Cards.cable40025 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Cable40025 = Cable40025 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40025 where
  runMessage msg a@(Cable40025 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      piles <- concatMapM (projectMap PlayerIdentityDiscard unDiscard) players
      cards <- filterM (canAttachPlayerUpgradeToAlly (allyId attrs)) [c | c <- piles, cardMatch (CardWithType UpgradeType <> CardWithPrintedCostAtMost 1) c]
      chooseOne you [targeting c $ putUpgradeOnAllyFromDiscard you (allyId attrs) c | c <- cards]
      pure a
    _ -> Cable40025 <$> runMessage msg attrs

cable40204 :: AllyCard Cable40204
cable40204 = ally Cable40204 Cards.cable40204 (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype Cable40204 = Cable40204 (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40204 where
  runMessage msg a@(Cable40204 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithTrait Trait.Superpower) SearchDrawOne ShuffleBackIn
      pure a
    _ -> Cable40204 <$> runMessage msg attrs

instance HasAbilities Cable40014 where
  getAbilities a@(Cable40014 _) =
    [limitedWindowAbility a 1 (W.EntersPlayThis) Response OwnsThis NoCost (runAbility a 1)]

instance HasAbilities Cable40015 where
  getAbilities a@(Cable40015 _) =
    [limitedWindowAbility a 1 (W.EntersPlayThis) Response OwnsThis NoCost (runAbility a 1)]

instance HasAbilities Cable40016 where
  getAbilities a@(Cable40016 _) =
    [limitedWindowAbility a 1 (W.PlayThis W.After) Response OwnsThis NoCost (runAbility a 1)]

instance HasAbilities Cable40025 where
  getAbilities a@(Cable40025 _) =
    [limitedWindowAbility a 1 (W.EntersPlayThis) HeroResponse OwnsThis NoCost (runAbility a 1)]

instance HasAbilities Cable40204 where
  getAbilities a@(Cable40204 _) =
    [limitedWindowAbility a 1 (W.PlayThis W.After) Response OwnsThis NoCost (runAbility a 1)]

instance HasModifiersFor Cable40204 where
  getModifiersFor _ (AllyRef aid) (Cable40204 attrs) | aid == allyId attrs = do
    traits <- filterM (\trait -> selectAny $ IdentityWithId (allyController attrs) <> IdentityWithTrait trait) [minBound .. maxBound]
    pure (TraitModifier <$> traits)
  getModifiersFor _ _ _ = pure []
