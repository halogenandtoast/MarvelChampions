module Marvel.Hero.Heroes.StarLord (
  starLord,
  starLordEffect,
  StarLord (..),
  StarLordEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait (Trait (Guardian))
import Marvel.Window qualified as W

starLord :: HeroCard StarLord
starLord =
  hero
    StarLord
    Cards.starLord
    (HP $ Static 10)
    (HandSize 5)
    (Thw 2)
    (Atk 2)
    (Def 1)

-- | "What could go wrong?" — Interrupt: When you play a card from your hand, deal
-- yourself 1 facedown encounter card → reduce the cost to play that card by 3. (Limit
-- once per round.) The window opens only for a card that costs resources.
whatCouldGoWrong :: Attrs Hero -> Ability
whatCouldGoWrong a =
  label "\"What could go wrong?\"" $
    limited (PerRound 1) $
      limitedWindowAbility
        a
        1
        (W.PlaysCardFromHand (IdentityWithId $ heroIdentityId a) AnyCard)
        Interrupt
        IsSelf
        DealEncounterCardCost
        (runAbility a 1)

instance HasAbilities StarLord where
  getAbilities (StarLord a) = [whatCouldGoWrong a]

newtype StarLord = StarLord (Attrs Hero)
  deriving anyclass (IsHero)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | Each ally you control gains the Guardian trait. While "What could go wrong?" is
-- unused this round, a card from hand it would make affordable counts as playable.
instance HasModifiersFor StarLord where
  getModifiersFor _ target (StarLord a) | isTarget a target = do
    used <- getUsedAbilities
    pure [MayReduceCostToPlay 3 | passesUseLimit ident used (whatCouldGoWrong a)]
   where
    ident = heroIdentityId a
  getModifiersFor _ (AllyRef aid) (StarLord a) = do
    yours <- selectAny (AllyWithId aid <> AllyControlledBy (IdentityWithId $ heroIdentityId a))
    pure [TraitModifier Guardian | yours]
  getModifiersFor _ _ _ = pure []

instance RunMessage StarLord where
  runMessage msg h@(StarLord attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      createEffect Cards.starLord attrs (IdentityEntity $ IdentityWithId ident)
      pure h
    _ -> StarLord <$> runMessage msg attrs

-- | The card being played costs 3 less: gone once it is played or its payment given up.
newtype StarLordEffect = StarLordEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

starLordEffect :: CardEffect StarLordEffect
starLordEffect =
  effectWith
    StarLordEffect
    Cards.starLord
    ((modifiersL .~ [ResourceCostReduction 3]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage StarLordEffect where
  runMessage msg e@(StarLordEffect attrs) = runQueueT $ case msg of
    IdentityMessage ident (PlayedCard _ _) -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    -- ponytail: any payment given up ends it; the only one under way is that card's
    AbortPayment _ -> do
      disableEffect (effectId attrs)
      pure e
    _ -> StarLordEffect <$> runMessage msg attrs
