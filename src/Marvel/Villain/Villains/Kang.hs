-- | The Once and Future Kang's villains: Kang (I) and Kang (III), The Conqueror, and the
-- four Kang (II), each in a game area of his own. The Expert Kang set is the same text
-- with other stats (TOAFK insert p.6), so each module builds both.
module Marvel.Villain.Villains.Kang where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attack
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.MainScheme.Cards qualified as MainSchemes
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (Choice (RunAbility))
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W

-- | Toughness: each Kang enters play with a tough status card.
kangWith :: (Attrs Villain -> a) -> CardDef -> Natural -> Sch -> Atk -> HP GameValue -> VillainCard a
kangWith f def stage sch atk hp = villainWith f def sch atk hp ((stageL .~ stage) . (toughL .~ True))

-- * Kang (I) and Kang (III), The Conqueror

newtype Kang = Kang (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

kang1 :: VillainCard Kang
kang1 = kangWith Kang Cards.kang1 1 (Sch 1) (Atk 2) (HP $ PerPlayer 12)

kang3 :: VillainCard Kang
kang3 = kangWith Kang Cards.kang3 3 (Sch 2) (Atk 2) (HP $ PerPlayer 20)

kang1Expert :: VillainCard Kang
kang1Expert = kangWith Kang Cards.kang1Expert 1 (Sch 1) (Atk 3) (HP $ PerPlayer 15)

kang3Expert :: VillainCard Kang
kang3Expert = kangWith Kang Cards.kang3Expert 3 (Sch 3) (Atk 3) (HP $ PerPlayer 25)

-- | Forced Interrupt: when Kang attacks you, either place 1 threat on the main scheme, or
-- he gets +2 ATK for this attack.
instance HasAbilities Kang where
  getAbilities (Kang a) =
    [ windowAbility a 1 (EnemyAttacked When (EnemyWithId $ EnemyVillainId $ villainId a) AnyIdentity) ForcedInterrupt NoCost $
        RunAbility (toTarget a) 1
    ]

-- | When Defeated (I): advance the main scheme to stage 2 at the end of the phase; with
-- no next villain in the villain deck, he leaves play, and nothing on him carries over
-- (TOAFK insert p.7). When Defeated (III): the players win.
instance RunMessage Kang where
  runMessage msg e@(Kang attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs, villainStage attrs == 1 -> do
      removeFromPlay attrs
      arrival <- stagesIn MainSchemes.kangsArrival =<< getAreaOfTarget ident
      for_ arrival (atEndOfPhase . nextMainScheme)
      pure e
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.EnemyAttack _ you <- windows] \you -> do
        scheme <- selectOne MainScheme
        chooseOne you $
          [labeled (countLabel "kangPlacesThreat" 1) (placeThreat s 1) | Just s <- [scheme]]
            <> [labeled (countLabel "kangGetsAttack" 2) (cardSpecific attrs "attackGets2" ())]
      pure e
    CardSpecific (isTarget attrs -> True) "attackGets2" _ ->
      pure . Kang $ attrs & attackingL . _Just . attackDamageL +~ 2
    _ -> Kang <$> runMessage msg attrs

-- | The main schemes of the stage in the area.
stagesIn :: (HasGame m) => CardDef -> AreaId -> m [MainSchemeId]
stagesIn stage aid = filterM (fmap (== Just (toCardCode stage)) . getMainSchemeCode) =<< getMainSchemesIn aid

-- * Kang (II)

-- | When Defeated: remove his stage 3 from the game; at the end of the phase, his
-- area's players join another game area.
kang2Defeated :: (ReverseQueue m) => CardDef -> Attrs Villain -> m ()
kang2Defeated stage3 attrs = do
  aid <- getAreaOfTarget (villainId attrs)
  removeFromPlay attrs
  traverse_ (removeFromGame . MainSchemeRef) =<< stagesIn stage3 aid
  atEndOfPhase $ cardSpecific GameRef "kangJoinAnotherArea" aid

newtype KangImmortus = KangImmortus (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

kangImmortus :: VillainCard KangImmortus
kangImmortus = kangWith KangImmortus Cards.kangImmortus 2 (Sch 2) (Atk 2) (HP $ Static 18)

kangImmortusExpert :: VillainCard KangImmortus
kangImmortusExpert = kangWith KangImmortus Cards.kangImmortusExpert 2 (Sch 3) (Atk 2) (HP $ Static 22)

-- | This villain cannot take damage while a minion is in play: in his game area.
instance HasModifiersFor KangImmortus where
  getModifiersFor _ (VillainRef vid) (KangImmortus a) | vid == villainId a = do
    aid <- getAreaOfTarget vid
    minions <- getMinionsIn aid
    pure [CannotTakeDamage | notNull minions]
  getModifiersFor _ _ _ = pure []

instance RunMessage KangImmortus where
  runMessage msg e@(KangImmortus attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> e <$ kang2Defeated MainSchemes.theChronopolis attrs
    _ -> KangImmortus <$> runMessage msg attrs

newtype KangIronLad = KangIronLad (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | Retaliate 1, a keyword of his card.
kangIronLad :: VillainCard KangIronLad
kangIronLad = kangWith KangIronLad Cards.kangIronLad 2 (Sch 1) (Atk 2) (HP $ Static 18)

kangIronLadExpert :: VillainCard KangIronLad
kangIronLadExpert = kangWith KangIronLad Cards.kangIronLadExpert 2 (Sch 2) (Atk 3) (HP $ Static 22)

instance RunMessage KangIronLad where
  runMessage msg e@(KangIronLad attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> e <$ kang2Defeated MainSchemes.inexorableFate attrs
    _ -> KangIronLad <$> runMessage msg attrs

newtype KangRamaTut = KangRamaTut (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

kangRamaTut :: VillainCard KangRamaTut
kangRamaTut = kangWith KangRamaTut Cards.kangRamaTut 2 (Sch 1) (Atk 2) (HP $ Static 18)

kangRamaTutExpert :: VillainCard KangRamaTut
kangRamaTutExpert = kangWith KangRamaTut Cards.kangRamaTutExpert 2 (Sch 2) (Atk 3) (HP $ Static 22)

-- | [star] This villain gets +1 ATK for each obligation in play: in his game area.
instance HasModifiersFor KangRamaTut where
  getModifiersFor _ (VillainRef vid) (KangRamaTut a) | vid == villainId a = do
    aid <- getAreaOfTarget vid
    n <- length <$> getObligationsIn aid
    pure [AttackModifier (fromIntegral n) | n > 0]
  getModifiersFor _ _ _ = pure []

instance RunMessage KangRamaTut where
  runMessage msg e@(KangRamaTut attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> e <$ kang2Defeated MainSchemes.theRealmOfRamaTut attrs
    _ -> KangRamaTut <$> runMessage msg attrs

newtype KangScarletCenturion = KangScarletCenturion (Attrs Villain)
  deriving anyclass (IsVillain, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

kangScarletCenturion :: VillainCard KangScarletCenturion
kangScarletCenturion = kangWith KangScarletCenturion Cards.kangScarletCenturion 2 (Sch 0) (Atk 3) (HP $ Static 18)

kangScarletCenturionExpert :: VillainCard KangScarletCenturion
kangScarletCenturionExpert = kangWith KangScarletCenturion Cards.kangScarletCenturionExpert 2 (Sch 1) (Atk 4) (HP $ Static 22)

-- | [star] This villain's attacks gain piercing.
instance HasModifiersFor KangScarletCenturion where
  getModifiersFor _ (VillainRef vid) (KangScarletCenturion a) | vid == villainId a = pure [KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage KangScarletCenturion where
  runMessage msg e@(KangScarletCenturion attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> e <$ kang2Defeated MainSchemes.thePresentFutureWar attrs
    _ -> KangScarletCenturion <$> runMessage msg attrs
