module Marvel.Villain.Villains.SmVenom where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

-- Damage is remembered until the attack has completed: a multi-hit attack gives
-- only one Vengeance response, and damage prevented or converted to chimes gives none.
data VenomMeta = VenomMeta
  { damagingAttackers :: [Source]
  , attacksThisTurn :: Natural
  , attacksAgainstVenomThisTurn :: Natural
  , damagingAttacksThisTurn :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmVenomVillain = SmVenomVillain (Attrs Villain `With` VenomMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance IsVillain SmVenomVillain where
  toVillainAttrs (SmVenomVillain (attrs `With` _)) = attrs
  overVillainAttrs f (SmVenomVillain (attrs `With` meta)) = SmVenomVillain (f attrs `With` meta)

venomVillain :: Attrs Villain -> SmVenomVillain
venomVillain = SmVenomVillain . (`With` VenomMeta [] 0 0 0)

smVenom1, smVenom2, smVenom3 :: VillainCard SmVenomVillain
smVenom1 = villainWith venomVillain Cards.smVenom1 (Sch 1) (Atk 2) (HP $ PerPlayer 17) (stageL .~ 1)
smVenom2 = villainWith venomVillain Cards.smVenom2 (Sch 2) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 2)
smVenom3 = villainWith venomVillain Cards.smVenom3 (Sch 2) (Atk 3) (HP $ PerPlayer 20) (stageL .~ 3)

instance HasModifiersFor SmVenomVillain

instance HasAbilities SmVenomVillain where
  getAbilities (SmVenomVillain (a `With` _)) =
    [windowAbility a 1 (VillainRevealed When (VillainWithId $ villainId a) RevealedFromVillain) WhenRevealed NoCost (runAbility a 1) | villainStage a >= 2]

instance RunMessage SmVenomVillain where
  runMessage msg e@(SmVenomVillain (attrs `With` meta)) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (Normal, 1) -> advance vid smVenom2
        (Expert, 2) -> advance vid smVenom3
        _ -> SmVenomVillain . (`With` meta) <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      if villainStage attrs == 2
        then do
          first <- selectJust FirstPlayer
          searchAndPutIntoPlayEncounterCard first (CardIs SideSchemes.smToothAndNail)
        else getPlayers >>= traverse_ (\you -> giveIdentityBoostCards you 2)
      pure e
    IdentityMessage _ BeginTurn -> pure $ SmVenomVillain (attrs `With` VenomMeta [] 0 0 0)
    BeginPhase VillainPhase -> pure $ SmVenomVillain (attrs `With` VenomMeta [] 0 0 0)
    CheckWindows windows -> do
      let hits =
            [ source
            | W.Window After (W.EnemyTookDamageFrom enemy damage source) <- windows
            , enemy == EnemyVillainId (villainId attrs)
            , damageAmount damage > 0
            , case damageSource damage of FromPlayerAttack _ -> True; FromAllyAttack _ -> True; _ -> False
            ]
          completed = [(source, you, enemies) | W.Window After (W.PlayerAttackResolved source you enemies) <- windows]
          marked = nub (damagingAttackers meta <> hits)
          against = [(source, you) | (source, you, enemies) <- completed, EnemyVillainId (villainId attrs) `elem` enemies, source `elem` marked]
          venomAttacks = length [() | (_, _, enemies) <- completed, EnemyVillainId (villainId attrs) `elem` enemies]
      blanked <- elem Blank <$> getModifiers attrs
      unless blanked $ for_ (zip [0 :: Natural ..] completed) \(index, (source, you, enemies)) ->
        when (EnemyVillainId (villainId attrs) `elem` enemies && source `elem` marked) do
          -- Owner ruling 2026-10-30: any player/ally attack against any enemy
          -- occupies the first-attack position, even if it dealt no damage.
          let cards = if villainStage attrs >= 3 && attacksThisTurn meta + index == 0 then 2 else 1
          forcedResponse attrs (giveIdentityBoostCards you cards)
      pure $ SmVenomVillain $ attrs `With` meta
        { damagingAttackers = filter (`notElem` [source | (source, _, _) <- completed]) marked
        , attacksThisTurn = attacksThisTurn meta + fromIntegral (length completed)
        , attacksAgainstVenomThisTurn = attacksAgainstVenomThisTurn meta + fromIntegral venomAttacks
        , damagingAttacksThisTurn = damagingAttacksThisTurn meta + fromIntegral (length against)
        }
    _ -> SmVenomVillain . (`With` meta) <$> runMessage msg attrs
   where
    advance vid card = do
      advanceVillain vid
      let SmVenomVillain (updated `With` _) = advanceVillainTo card attrs
      pure $ SmVenomVillain (updated `With` meta)
