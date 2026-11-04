module Marvel.FearNoEvil.Bullseye where

import Marvel.Prelude
import Marvel.Card
import Marvel.Ability
import Marvel.Cost
import Marvel.Hp
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Stats
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Id
import Marvel.GameValue
import Marvel.SideScheme.Types
import Marvel.Modifier
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

newtype BullseyeTreachery = BullseyeTreachery (Attrs Treachery `With` Bool)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
instance IsTreachery BullseyeTreachery where
  toTreacheryAttrs (BullseyeTreachery (a `With` _)) = a
bullseyeTreacheries :: [TreacheryCard BullseyeTreachery]
bullseyeTreacheries = map (treachery (\a -> BullseyeTreachery (a `With` False))) [Cards.street60074,Cards.street60075]
instance RunMessage BullseyeTreachery where
  runMessage msg e@(BullseyeTreachery (a `With` movedToBoost)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "60074" -> do
          hero <- identityMatches HeroIdentity you
          if not hero then dealIndirectDamage you a 2 else do
            bullseye <- villainNamed "Bullseye"
            allies <- selectList $ AllyControlledBy $ IdentityWithId you
            chooseOne you $
              [targeting (enemyRef $ EnemyVillainId vid) (villainActivatesAgainst vid you) | vid <- catMaybes [bullseye]]
              <> [targeting (AllyRef ally) (removeFromGame ally) | ally <- allies]
        _ -> do
          supports <- selectList $ SupportControlledBy (IdentityWithId you) <> SupportWithTrait T.Persona
          villain <- selectOne ActiveVillain
          chooseOne you $
            [targeting (enemyRef $ EnemyVillainId vid) (giveCardAsBoost a vid >> cardSpecific a "movedToBoost" ()) | vid <- catMaybes [villain]]
            <> [targeting (SupportRef support) (removeFromGame support) | support <- supports]
      pure e
    CardSpecific (isTarget a -> True) "movedToBoost" _ -> pure $ BullseyeTreachery (a `With` True)
    TreacheryMessage tid (ResolvedTreachery you) | tid == treacheryId a, movedToBoost -> do
      resolveTreacheryAsBoost tid you (MkEncounterCard (CardId $ unTreacheryId tid) $ getCardDef a)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      attacking <- isJust <$> getEnemyAttack enemy
      if toCardCode a == "60075" then when attacking $ attackGains enemy Overkill else do
        bullseye <- villainNamed "Bullseye"
        when (attacking && maybe False ((== enemy) . EnemyVillainId) bullseye) $ dealBoost (enemyRef enemy) >> attackGains enemy Piercing
      pure e
    _ -> (\attrs -> BullseyeTreachery (attrs `With` movedToBoost)) <$> runMessage msg a

newtype DerangedBloodlust = DerangedBloodlust (Attrs SideScheme)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass (IsSideScheme,HasModifiersFor)
derangedBloodlust :: SideSchemeCard DerangedBloodlust
derangedBloodlust = sideScheme DerangedBloodlust Cards.street60073 (Static 4)
instance RunMessage DerangedBloodlust where
  runMessage msg e@(DerangedBloodlust a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      bullseye <- villainNamed "Bullseye"
      stage <- maybe (pure 0) getVillainStage bullseye
      placeThreatOn a sid stage
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCard a of
        EncounterCard card -> do
          consumeBoostCard card
          enemyActivationAfterCurrent enemy $ revealFoundEncounterCard you card
        _ -> pure ()
      pure e
    _ -> DerangedBloodlust <$> runMessage msg a

newtype LadyBullseye = LadyBullseye (Attrs Minion `With` Bool)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)
  deriving anyclass HasModifiersFor
instance IsMinion LadyBullseye where
  toMinionAttrs (LadyBullseye (a `With` _)) = a
  overMinionAttrs f (LadyBullseye (a `With` opened)) = LadyBullseye (f a `With` opened)
ladyBullseye :: MinionCard LadyBullseye
ladyBullseye = minion (\a -> LadyBullseye (a `With` False)) Cards.street60072 (Sch 2) (Atk 1) (HP 6)
instance HasAbilities LadyBullseye where
  getAbilities (LadyBullseye (a `With` _)) =
    [windowAbility a 1 (W.EnemyAboutToResolveBoostCards $ EnemyWithId (toEnemyId a) <> UndefendedEnemy) ForcedInterrupt NoCost (runAbility a 1)]
instance RunMessage LadyBullseye where
  runMessage msg e@(LadyBullseye (a `With` opened)) = runQueueT $ case msg of
    MinionMessage mid MinionFlipBoostCards | mid == minionId a, not opened -> do
      checkWindows [W.Window W.When $ W.ResolvingEnemyBoostCards $ toEnemyId a]
      minionStep mid MinionFlipBoostCards
      pure $ LadyBullseye (a `With` True)
    RanAbility _ (isTarget a -> True) 1 _ _ -> dealBoost (minionId a) >> pure e
    MinionMessage mid step | mid == minionId a, step `elem` [MinionEndAttack,MinionSchemed] -> do
      updated <- runMessage msg a
      pure $ LadyBullseye (updated `With` False)
    _ -> (\attrs -> LadyBullseye (attrs `With` opened)) <$> runMessage msg a
