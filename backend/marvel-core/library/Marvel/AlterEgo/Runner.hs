{-# OPTIONS_GHC -Wno-orphans #-}
module Marvel.AlterEgo.Runner
  ( module X
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Types as X
import Marvel.Card.Def
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
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
          push
            (IdentityMessage ident
            $ IdentityHealed
            . unRec
            $ alterEgoBaseRecovery a
            )
          pure a
        _ -> pure a
    _ -> pure a
