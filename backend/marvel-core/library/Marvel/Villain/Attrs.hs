{-# LANGUAGE TemplateHaskell #-}
module Marvel.Villain.Attrs
  ( module Marvel.Villain.Attrs
  , module X
  ) where

import Marvel.Prelude

import Marvel.Card.Builder
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity as X
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id as X (VillainId)
import Marvel.Message
import Marvel.Stats

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

instance RunMessage VillainAttrs where
  runMessage msg attrs = case msg of
    VillainMessage villainId msg' | villainId == toId attrs ->
      runVillainMessage msg' attrs
    _ -> pure attrs
