{-# LANGUAGE UndecidableInstances #-}
module Marvel.Minion.Minions.HydraMercenary where

import Marvel.Prelude

import Marvel.Minion.Attrs
import qualified Marvel.Minion.Cards as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Source
import Marvel.Stats
import Marvel.Target

hydraMercenary :: MinionCard HydraMercenary
hydraMercenary = minion
  HydraMercenary
  Cards.hydraMercenary
  (Sch 0)
  (Atk 1)
  (HP 3)

newtype HydraMercenary = HydraMercenary MinionAttrs
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage HydraMercenary where
  runMessage msg (HydraMercenary attrs) = HydraMercenary <$> runMessage msg attrs
