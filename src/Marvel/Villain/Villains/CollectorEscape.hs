-- | Escape the Museum's villain (The Galaxy's Most Wanted): Collector (A1), or (B1) for
-- expert play, each with a Wounded side (A2, B2) of infinite hit points he flips to in
-- place of being defeated, and back from as the round ends.
module Marvel.Villain.Villains.CollectorEscape where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id (SchemeId (..))
import Marvel.MainScheme.Cards qualified as MainSchemes
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait (Trait (Wounded))
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype CollectorEscape = CollectorEscape (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | The Wounded sides print ∞ hit points; theirs here are the side's they flip from, the
-- dial being set to them as he flips back.
collectorA1, collectorA2, collectorB1, collectorB2 :: VillainCard CollectorEscape
collectorA1 = villain CollectorEscape Cards.collectorA1 (Sch 1) (Atk 1) (HP $ PerPlayer 8)
collectorA2 = villain CollectorEscape Cards.collectorA2 (Sch 0) (Atk 0) (HP $ PerPlayer 8)
collectorB1 = villain CollectorEscape Cards.collectorB1 (Sch 2) (Atk 2) (HP $ PerPlayer 10)
collectorB2 = villain CollectorEscape Cards.collectorB2 (Sch 2) (Atk 2) (HP $ PerPlayer 10)

isWounded :: Attrs Villain -> Bool
isWounded a = Wounded `member` cdTraits (getCardDef a)

-- | The other side of his card.
otherFace :: Attrs Villain -> VillainCard CollectorEscape
otherFace a = case toCardCode (getCardDef a) of
  "16080a" -> collectorA2
  "16080b" -> collectorA1
  "16081a" -> collectorB2
  _ -> collectorB1

-- | The main scheme's current stage number: 1 to 3, by Escape the Museum's main schemes.
mainSchemeStage :: (HasGame m) => m Natural
mainSchemeStage = do
  scheme <- selectOne MainScheme
  code <- case scheme of
    Just (SchemeMainSchemeId mid) -> getMainSchemeCode mid
    _ -> pure Nothing
  pure $ fromMaybe 1 $ headMay [n | (s, n) <- zip stages [1 ..], code == Just (toCardCode s)]
 where
  stages = [MainSchemes.theMissingMilano, MainSchemes.lostInTheMuseum, MainSchemes.theGreatEscape]

-- | [star] (A1, B1): +X SCH and +X ATK, X the main scheme's current stage number.
-- Wounded (A2, B2): infinite hit points, so he cannot be defeated.
instance HasModifiersFor CollectorEscape where
  getModifiersFor _ (VillainRef vid) (CollectorEscape a)
    | vid == villainId a =
        if isWounded a
          then pure [InfiniteHitPoints]
          else mainSchemeStage <&> \x -> [SchemeModifier x, AttackModifier x]
  getModifiersFor _ _ _ = pure []

-- | Wounded, Forced Interrupt: when the round ends, flip this card, then set his hit
-- point dial to his printed hit points.
instance HasAbilities CollectorEscape where
  getAbilities (CollectorEscape a)
    | isWounded a = [windowAbility a 1 W.RoundEnds ForcedInterrupt NoCost $ runAbility a 1]
    | otherwise = []

-- | Forced Interrupt (A1, B1): when he would be defeated, remove 3[per hero] threat from
-- the main scheme and flip this card instead.
instance RunMessage CollectorEscape where
  runMessage msg e@(CollectorEscape attrs) = runQueueT $ case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainWouldBeDefeated | not (isWounded attrs) -> do
        cancelVillainDefeat ident
        n <- fromGameValue (PerPlayer 3)
        scheme <- selectOne MainScheme
        for_ scheme \s -> thwartScheme s attrs (fromIntegral n)
        flipVillain ident
        pure e
      FlipVillain -> pure $ flipVillainTo (otherFace attrs) attrs
      _ -> CollectorEscape <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      flipVillain (villainId attrs)
      villainStep (villainId attrs) SetVillainHp
      pure e
    _ -> CollectorEscape <$> runMessage msg attrs
