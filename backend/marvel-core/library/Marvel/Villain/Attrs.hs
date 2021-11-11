{-# LANGUAGE TemplateHaskell #-}
module Marvel.Villain.Attrs
  ( module Marvel.Villain.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Boost
import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Entity as X
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Id as X (VillainId)
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Stats
import Marvel.Target

class IsVillain a

type VillainCard a = CardBuilder VillainId a

villain :: (VillainAttrs -> a) -> CardDef -> Sch -> Atk -> HP -> VillainCard a
villain f cardDef sch atk startingHp = CardBuilder
  { cbCardCode = toCardCode cardDef
  , cbCardBuilder = \ident -> f $ VillainAttrs
    { villainId = ident
    , villainCardDef = cardDef
    , villainStartingHp = startingHp
    , villainMaxHp = 1
    , villainHp = 1
    , villainScheme = sch
    , villainAttack = atk
    , villainStunned = False
    , villainBoostCards = mempty
    , villainBoost = 0
    }
  }

data VillainAttrs = VillainAttrs
  { villainId :: VillainId
  , villainCardDef :: CardDef
  , villainHp :: Int
  , villainStartingHp :: HP
  , villainMaxHp :: Int
  , villainScheme :: Sch
  , villainAttack :: Atk
  , villainStunned :: Bool
  , villainBoostCards :: [EncounterCard]
  , villainBoost :: Natural
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeLensesWith suffixedFields ''VillainAttrs

instance Entity VillainAttrs where
  type EntityId VillainAttrs = VillainId
  type EntityAttrs VillainAttrs = VillainAttrs
  toId = villainId
  toAttrs = id

runVillainMessage
  :: MonadGame env m => VillainMessage -> VillainAttrs -> m VillainAttrs
runVillainMessage msg attrs = case msg of
  SetVillainHp -> do
    hp <- fromGameValue (unHp $ villainStartingHp attrs)
    pure $ attrs & hpL .~ hp & maxHpL .~ hp
  VillainDamaged _ n -> pure $ attrs & hpL %~ max 0 . subtract (fromIntegral n)
  VillainStunned _ -> pure $ attrs & stunnedL .~ True
  VillainSchemes -> do
    pushAll
      [ DealBoost (toTarget attrs)
      , VillainMessage (toId attrs) VillainFlipBoostCards
      , VillainMessage (toId attrs) VillainSchemed
      ]
    pure attrs
  VillainSchemed -> do
    mainScheme <- selectJust MainScheme
    case mainScheme of
      SchemeMainSchemeId mainSchemeId -> do
        let threat = unSch (villainScheme attrs) + villainBoost attrs
        push (MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat threat)
        pure $ attrs & boostL .~ 0
  DealtBoost c -> pure $ attrs & boostCardsL %~ (c :)
  VillainFlipBoostCards -> do
    let
      boost = foldr
        ((+) . boostCount . cdBoostIcons . ecCardDef)
        0
        (villainBoostCards attrs)
    pushAll $ map DiscardedEncounterCard (villainBoostCards attrs)
    pure $ attrs & boostCardsL .~ mempty & boostL .~ boost

instance RunMessage VillainAttrs where
  runMessage msg attrs = case msg of
    VillainMessage villainId msg' | villainId == toId attrs ->
      runVillainMessage msg' attrs
    _ -> pure attrs

instance IsTarget VillainAttrs where
  toTarget = VillainTarget . villainId
