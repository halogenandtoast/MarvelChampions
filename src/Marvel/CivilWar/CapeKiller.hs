module Marvel.CivilWar.CapeKiller where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype CapeKiller = CapeKiller (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

capeKiller :: MinionCard CapeKiller
capeKiller = minion CapeKiller C.cw56105 (Sch 1) (Atk 2) (HP 4)

instance HasAbilities CapeKiller where
  getAbilities (CapeKiller a) =
    [windowAbility a 1 (W.EnemyAttacked W.When
      (EnemyWithId (EnemyMinionId $ minionId a) <> EnemyAttackingCharacter (IdentityCharacters $ IdentityWithTrait T.Unregistered))
      (IdentityWithTrait T.Unregistered)) ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage CapeKiller where
  runMessage msg e@(CapeKiller a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ changeAttack (minionId a) 2
    _ -> CapeKiller <$> runMessage msg a

newtype BringThemIn = BringThemIn (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

bringThemIn :: TreacheryCard BringThemIn
bringThemIn = treachery BringThemIn C.cw56107

instance RunMessage BringThemIn where
  runMessage msg e@(BringThemIn a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      candidates <- asYou you $ selectList $ IdentityWithTrait T.Unregistered <> UnexhaustedIdentity
      if null candidates then dealUnregisteredSuperForEnemyTeam you a else traverse_ exhaust candidates
      pure e
    _ -> BringThemIn <$> runMessage msg a

newtype ArrestOrder = ArrestOrder (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

arrestOrder :: SideSchemeCard ArrestOrder
arrestOrder = sideScheme ArrestOrder C.cw56108 (Static 3)

instance RunMessage ArrestOrder where
  runMessage msg (ArrestOrder a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- getDefeatedBy a
      discard <- asYou you getEncounterDiscard
      for_ (headMay $ filter (cardMatch $ CardWithType MinionType <> CardWithTrait T.Shield) discard) $
        dealFoundEncounterCard you
      ArrestOrder <$> runMessage msg a
    _ -> ArrestOrder <$> runMessage msg a
