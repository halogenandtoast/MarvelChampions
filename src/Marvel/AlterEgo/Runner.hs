{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.AlterEgo.Runner
  ( module X
  , getModifiedRecovery
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Types as X
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Modifier
import Marvel.Message
import Marvel.Question
import Marvel.Trait
import Marvel.Stats
import Marvel.Queue

instance HasAbilities AlterEgo where
  getAbilities (AlterEgo a) = getAbilities a <> basicAbilities
    where basicAbilities = [ability a 200 Basic NoCriteria ExhaustCost Recover]

instance HasTraits AlterEgo where
  getTraits = pure . cdTraits . getCardDef

instance HasModifiersFor AlterEgo where
  getModifiersFor source target (AlterEgo a) = getModifiersFor source target a


instance RunMessage AlterEgo where
  runMessage msg (AlterEgo a) = AlterEgo <$> runMessage msg a

instance RunMessage (Attrs AlterEgo) where
  runMessage msg a = case msg of
    IdentityMessage ident (SideMessage msg') | ident == alterEgoIdentityId a ->
      case msg' of
        Recovered -> do
          rec <- getModifiedRecovery a
          push $ IdentityMessage ident $ IdentityHealed rec
          pure a
        _ -> pure a
    _ -> pure a

getModifiedRecovery :: (HasGame m) => Attrs AlterEgo -> m Natural
getModifiedRecovery attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unRec $ alterEgoBaseRecovery attrs) modifiers
 where
  applyModifier (RecoveryModifier n) = (+ n)
  applyModifier _ = id
