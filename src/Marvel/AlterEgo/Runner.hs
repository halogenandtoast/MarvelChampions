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
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Question
import Marvel.Trait
import Marvel.Stats
import Marvel.Window qualified as W ()

instance HasAbilities AlterEgo where
  getAbilities (AlterEgo a) = getAbilities a <> basicAbilities
    where basicAbilities = [ability a 200 Basic (SelfMatches (IdentityWithAnyDamage <> IdentityCanRecover)) ExhaustCost Recover]

instance HasTraits AlterEgo where
  getTraits = pure . cdTraits . getCardDef

instance HasModifiersFor AlterEgo where
  getModifiersFor source target (AlterEgo a) = getModifiersFor source target a


instance RunMessage AlterEgo where
  runMessage msg (AlterEgo a) = AlterEgo <$> runMessage msg a

instance RunMessage (Attrs AlterEgo) where
  runMessage msg a = runQueueT $ case msg of
    IdentityMessage ident (SideMessage msg') | ident == alterEgoIdentityId a ->
      case msg' of
        Recovered -> do
          heal ident =<< getModifiedRecovery a
          pure a
        RecoveredWithBonus n -> do
          heal ident . (+ n) =<< getModifiedRecovery a
          pure a
        _ -> pure a
    _ -> pure a

getModifiedRecovery :: (HasGame m) => Attrs AlterEgo -> m Natural
getModifiedRecovery a = do
  modifiers <- getModifiers (alterEgoIdentityId a)
  pure $ subtractNatural (sum [n | RecoveryPenalty n <- modifiers]) (unRec (alterEgoBaseRecovery a) + sum [n | RecoveryModifier n <- modifiers])
