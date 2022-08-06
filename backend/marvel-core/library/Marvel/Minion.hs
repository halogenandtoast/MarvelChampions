module Marvel.Minion where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Data.Typeable
import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Minion.Attrs
import Marvel.Minion.Minions
import Marvel.Trait
import Text.Show qualified

data Minion = forall a. IsMinion a => Minion a

instance Show Minion where
  show (Minion a) = show a

instance ToJSON Minion where
  toJSON (Minion a) = toJSON a

instance Eq Minion where
  (Minion (a :: a)) == (Minion (b :: b)) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

instance FromJSON Minion where
  parseJSON v = flip (withObject "Minion") v $ \o -> do
    cardDef <- o .: "minionCardDef"
    withMinionCardCode (cdCardCode cardDef) $ \(_ :: MinionCard a) -> Minion <$> parseJSON @a v

withMinionCardCode
  :: CardCode
  -> (forall a. IsMinion a => MinionCard a -> r)
  -> r
withMinionCardCode cCode f =
  case lookup cCode allMinions of
    Nothing -> error "invalid minion"
    Just (SomeMinionCard a) -> f a

data SomeMinionCard = forall a. IsMinion a => SomeMinionCard (MinionCard a)

liftMinionCard :: (forall a . MinionCard a -> b) -> SomeMinionCard -> b
liftMinionCard f (SomeMinionCard a) = f a

someMinionCardCode :: SomeMinionCard -> CardCode
someMinionCardCode = liftMinionCard cbCardCode

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
      -- , SomeMinionCard advancedUltronDrone
      , SomeMinionCard killmonger
      , SomeMinionCard titania
      , SomeMinionCard vulture
      , SomeMinionCard whiplash
      -- , SomeMinionCard yonRogg
      -- , SomeMinionCard madameHydra
      -- , SomeMinionCard hydraSoldier
      -- , SomeMinionCard modok
      ]

lookupMinion :: CardCode -> IdentityId -> MinionId -> Minion
lookupMinion cardCode identityId minionId = case lookup cardCode allMinions of
  Just (SomeMinionCard a) -> Minion $ cbCardBuilder a (identityId, minionId)
  Nothing -> error $ "Invalid card code for minion " <> show cardCode

getMinionDamage :: Minion -> Natural
getMinionDamage = minionDamage . toAttrs

getMinionEngagedIdentity :: Minion -> IdentityId
getMinionEngagedIdentity = minionEngagedIdentity . toAttrs

getMinionPrintedHitPoints :: Minion -> HP Natural
getMinionPrintedHitPoints = minionHitPoints . toAttrs

minionAttackDetails :: Minion -> Maybe Attack
minionAttackDetails = minionAttacking . toAttrs

instance Entity Minion where
  type EntityId Minion = MinionId
  type EntityAttrs Minion = MinionAttrs
  toId = toId . toAttrs
  toAttrs (Minion a) = toAttrs a

instance RunMessage Minion where
  runMessage msg (Minion a) = Minion <$> runMessage msg a

instance HasAbilities Minion where
  getAbilities (Minion a) = getAbilities a

instance HasModifiersFor Minion where
  getModifiersFor source target (Minion a) = getModifiersFor source target a

instance HasTraits Minion where
  getTraits m = do
    modifiers <- getModifiers m
    let traits = cdTraits $ getCardDef m
    pure $ foldr applyModifier traits modifiers
   where
    applyModifier (TraitModifier t) = HashSet.insert t
    applyModifier _ = id

instance IsSource Minion where
  toSource = MinionSource . toId

instance IsTarget Minion where
  toTarget = MinionTarget . toId

instance IsCard Minion where
  toCard = toCard . toAttrs

instance HasCardDef Minion where
  getCardDef = getCardDef . toAttrs
