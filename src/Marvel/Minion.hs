{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Minion where

import Marvel.Prelude

import Marvel.Card
import Marvel.Id
import Marvel.Minion.Minions
import Marvel.Minion.Types

instance FromJSON Minion where
  parseJSON = withObject "Minion" $ \o -> do
    cardDef <- o .: "minionCardDef"
    withMinionCardCode (cdCardCode cardDef) $
      \(_ :: MinionCard a) -> Minion <$> parseJSON @a (Object o)

withMinionCardCode ::
  CardCode -> (forall a. (IsMinion a) => MinionCard a -> r) -> r
withMinionCardCode cCode f = case lookup cCode allMinions of
  Nothing -> error "invalid minion"
  Just (SomeMinionCard a) -> f a

allMinions :: HashMap CardCode SomeMinionCard
allMinions =
  fromList $
    map
      (toFst someMinionCardCode)
      [ SomeMinionCard hydraMercenary
      , SomeMinionCard sandman
      , SomeMinionCard shocker
      , SomeMinionCard hydraBomber
      , SomeMinionCard armoredGuard
      , SomeMinionCard weaponsRunner
      , SomeMinionCard radioactiveMan
      , SomeMinionCard whirlwind
      , SomeMinionCard tigerShark
      , SomeMinionCard melter
      , SomeMinionCard drone
      , -- , SomeMinionCard advancedUltronDrone
        SomeMinionCard killmonger
      , SomeMinionCard titania
      , SomeMinionCard vulture
      , SomeMinionCard whiplash
      , SomeMinionCard yonRogg
      -- , SomeMinionCard madameHydra
      -- , SomeMinionCard hydraSoldier
      -- , SomeMinionCard modok
      ]

lookupMinion :: CardCode -> IdentityId -> MinionId -> Minion
lookupMinion cardCode identityId minionId = case lookup cardCode allMinions of
  Just (SomeMinionCard a) -> Minion $ cbCardBuilder a (identityId, minionId)
  Nothing -> error $ "Invalid card code for minion " <> show cardCode
