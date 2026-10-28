-- | Hela (The Mad Titan's Shadow, scenario 4): one villain card of two sides, Mystic
-- (A1, or B1 for expert play) and Wounded (A2, B2). Flipping either way is resolved as
-- advancing a stage: her status cards stay, her hit points are reset (MTS rulebook
-- p.20).
module Marvel.Villain.Villains.Hela (
  helaA1,
  helaA2,
  helaB1,
  helaB2,
  Hela (..),
  odinAttachedToMainScheme,
) where

import Marvel.Prelude

import Data.Aeson (Result (..), fromJSON)
import Marvel.Ability
import Marvel.Ally.Cards qualified as Allies
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id (SchemeId (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait (Trait (Mystic))
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype Hela = Hela (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | The Wounded sides print ∞ hit points; theirs here are the Mystic side's, the dial
-- meaningless under infinite hit points.
helaA1, helaA2, helaB1, helaB2 :: VillainCard Hela
helaA1 = villain Hela Cards.helaA1 (Sch 1) (Atk 1) (HP $ PerPlayer 8)
helaA2 = villain Hela Cards.helaA2 (Sch 0) (Atk 0) (HP $ PerPlayer 8)
helaB1 = villain Hela Cards.helaB1 (Sch 2) (Atk 2) (HP $ PerPlayer 9)
helaB2 = villain Hela Cards.helaB2 (Sch 1) (Atk 1) (HP $ PerPlayer 9)

isMystic :: Attrs Villain -> Bool
isMystic a = Mystic `member` cdTraits (getCardDef a)

-- | The other side of her card.
otherFace :: Attrs Villain -> VillainCard Hela
otherFace a = case toCardCode (getCardDef a) of
  "21136a" -> helaA2
  "21136b" -> helaA1
  "21137a" -> helaB2
  _ -> helaB1

-- | The hit points her Mystic side gets for each side scheme in the victory display,
-- per player: 2 (A1), 3 (B1).
perSideScheme :: Attrs Villain -> Natural
perSideScheme a = if toCardCode (getCardDef a) `elem` ["21137a", "21137b"] then 3 else 2

-- | Whether Odin is attached to a main scheme (Odin's Torment holds him faceup).
odinAttachedToMainScheme :: (HasGame m) => m Bool
odinAttachedToMainScheme = do
  schemes <- selectList MainScheme
  held <- concat <$> for [m | SchemeMainSchemeId m <- schemes] getMainSchemeHeldCards
  pure $ any (sameCard Allies.odinCaptive . getCardDef) held || any (sameCard Allies.odinKing . getCardDef) held

-- | Mystic (A1, B1), [star]: Hela gets +1 SCH, +1 ATK and +2[per hero] (B1: +3[per
-- hero]) hit points for each side scheme in the victory display. Wounded (A2, B2): Hela
-- cannot be defeated (infinite hit points).
instance HasModifiersFor Hela where
  getModifiersFor _ (VillainRef vid) (Hela a)
    | vid == villainId a =
        if isMystic a
          then do
            n <- sideSchemesInVictoryDisplay
            players <- fromIntegral <$> getStartingPlayerCount
            pure $ if n > 0 then [SchemeModifier n, AttackModifier n, HitPointModifier (perSideScheme a * players * n)] else []
          else pure [InfiniteHitPoints]
  getModifiersFor _ _ _ = pure []

-- | Wounded, Forced Response: after a side scheme is defeated, flip Hela to her Mystic
-- side.
instance HasAbilities Hela where
  getAbilities (Hela a)
    | isMystic a = []
    | otherwise = [windowAbility a 1 (W.SideSchemeDefeated W.After AnySideScheme) ForcedResponse NoCost $ runAbility a 1]

-- | Odin's Torment, Forced Interrupt: when Hela would be defeated, if Odin is attached
-- to the main scheme, discard each attachment from Hela and flip her to her Wounded side
-- instead (only Hela can be its villain: it is answered here). With Odin rescued, her
-- defeat is the players' win, her stage being her deck's last.
instance RunMessage Hela where
  runMessage msg e@(Hela attrs) = runQueueT $ case msg of
    VillainMessage ident msg' | ident == villainId attrs -> case msg' of
      VillainWouldBeDefeated | isMystic attrs -> do
        captive <- odinAttachedToMainScheme
        if captive
          then do
            cancelVillainDefeat ident
            -- every card attached to her, a player's upgrade on her too
            for_ (villainAttachments attrs) (removeFromPlay . AttachmentRef)
            for_ (villainUpgrades attrs) (removeFromPlay . UpgradeRef)
            flipVillain ident
            villainStep ident SetVillainHp
            pure e
          else Hela <$> runMessage msg attrs
      -- Wounded: "Hela cannot be defeated", by any means
      VillainWouldBeDefeated -> do
        cancelVillainDefeat ident
        pure e
      FlipVillain -> pure $ flipVillainTo (otherFace attrs) attrs
      _ -> Hela <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      flipVillain (villainId attrs)
      villainStep (villainId attrs) SetVillainHp
      pure e
    -- "gets +X hit points": a side scheme reaching the victory display while she is
    -- Mystic raises her dial and its maximum at once (RR Hit Points)
    AddToVictoryDisplay (SideSchemeRef _) | isMystic attrs -> do
      before <- sideSchemesInVictoryDisplay
      cardSpecific attrs "victoryHitPoints" before
      pure e
    CardSpecific (isTarget attrs -> True) "victoryHitPoints" (fromJSON -> Success before) | isMystic attrs -> do
      now <- sideSchemesInVictoryDisplay
      players <- fromIntegral <$> getStartingPlayerCount
      let added = subtractNatural before now
      when (added > 0) $ changeVillainHitPoints (villainId attrs) (fromIntegral $ perSideScheme attrs * players * added)
      pure e
    _ -> Hela <$> runMessage msg attrs
