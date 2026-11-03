module Marvel.CivilWar.Atlanteans where

import Marvel.Prelude
import Control.Monad (replicateM_)
import Data.List (nub)
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types

newtype AtlanteanGuard = AtlanteanGuard (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

atlanteanGuard :: MinionCard AtlanteanGuard
atlanteanGuard = minion AtlanteanGuard C.cw56165 (Sch 1) (Atk 2) (HP 3)

instance RunMessage AtlanteanGuard where
  runMessage msg e@(AtlanteanGuard a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> e <$ discardTopOfDeck you 3
    _ -> AtlanteanGuard <$> runMessage msg a

newtype Atlanteans = Atlanteans (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

atlanteans :: TreacheryCard Atlanteans
atlanteans = treachery Atlanteans C.cw56166

instance RunMessage Atlanteans where
  runMessage msg e@(Atlanteans a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      discardFromDeck you 3 a
      pure $ Atlanteans a {treacheryResolver = Just you}
    WithDiscarded (isTarget a -> True) FromDeck cards -> do
      let n = fromIntegral $ length $ nub $ map (cdCardType . getCardDef) cards
      when (n > 0) $ for_ (treacheryResolver a) $ \you -> do
        hero <- asYou you $ identityMatches HeroIdentity you
        if hero then dealIndirectDamage you a n else do
          schemes <- asYou you $ selectList EncounterMainScheme
          for_ schemes $ \scheme -> placeThreatOn a scheme n
      pure e
    _ -> Atlanteans <$> runMessage msg a

newtype AtlantisAttacks = AtlantisAttacks (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

atlantisAttacks :: SideSchemeCard AtlantisAttacks
atlantisAttacks = sideScheme AtlantisAttacks C.cw56167 (Static 3)

instance RunMessage AtlantisAttacks where
  runMessage msg e@(AtlantisAttacks a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      you <- maybe getYou pure (sideSchemeRevealedBy a)
      n <- fromIntegral <$> fromGameValue (PerPlayer 1)
      discardFromDeck you n a
      pure e
    WithDiscarded (isTarget a -> True) FromDeck cards -> do
      let n = length $ nub $ map (cdCardType . getCardDef) cards
      replicateM_ n $ placeSideSchemeAccelerationToken $ sideSchemeId a
      pure e
    _ -> AtlantisAttacks <$> runMessage msg a
