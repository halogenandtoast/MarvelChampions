module Marvel.FearNoEvil.Cops where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.FearNoEvil.Cards qualified as Cards
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype Police = Police (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsMinion
police :: [MinionCard Police]
police = [minion Police Cards.street60182 (Sch 0) (Atk 2) (HP 3), minion Police Cards.street60183 (Sch 1) (Atk 2) (HP 4)]
instance HasModifiersFor Police
instance HasAbilities Police where
  getAbilities (Police a) =
    [windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId a) WhenRevealed NoCost (runAbility a 1)]
    <> [windowAbility a 2 (W.EnemyWouldScheme (EnemyWithId $ toEnemyId a) AnyIdentity) ForcedInterrupt NoCost (runAbility a 2) | toCardCode a == "60182"]
instance RunMessage Police where
  runMessage msg e@(Police a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      who <- maybe (pure you) (fmap (fromMaybe you) . getSourceController) $ headMay [s | W.DefeatedMinion _ _ s _ <- windows]
      if toCardCode a == "60182" then selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn a mid 2)
        else dealEncounterCard who
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      insteadOf (\case MinionMessage mid MinionBeginScheme -> mid == minionId a; MinionMessage mid (MinionSchemesAgainst _) -> mid == minionId a; _ -> False) $
        selectOne ActiveVillain >>= traverse_ (minionAttacksEnemy $ minionId a)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a == "60183" -> getBoostedPlayer enemy >>= \you -> dealIndirectDamage you a 2 >> pure e
    _ -> Police <$> runMessage msg a

newtype PoliceCordon = PoliceCordon (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsSideScheme PoliceCordon
policeCordon :: SideSchemeCard PoliceCordon
policeCordon = sideScheme PoliceCordon Cards.street60185 (Static 3)
instance HasModifiersFor PoliceCordon where
  getModifiersFor _ target _ = case target of
    MinionRef mid -> do
      cop <- minionMatches (MinionWithTrait T.Police) mid
      pure [KeywordModifier key | cop, key <- [Guard,Patrol]]
    _ -> pure []
instance RunMessage PoliceCordon where
  runMessage msg e@(PoliceCordon a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> getYou >>= \you -> searchAndRevealEncounterCard you (CardWithType MinionType <> CardWithTrait T.Police) >> pure e
    _ -> PoliceCordon <$> runMessage msg a

newtype Wanted = Wanted (Attrs Obligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsObligation, HasModifiersFor)
wanted :: ObligationCard Wanted
wanted = obligation Wanted Cards.street60184
instance HasAbilities Wanted where
  getAbilities (Wanted a) = [ability a 1 HeroAction OwnsThis NoCost (runAbility a 1)]
instance RunMessage Wanted where
  runMessage msg e@(Wanted a) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> pure $ Wanted a {obligationOwner = Just you}
    ObligationMessage oid ResolvedObligation{} | oid == obligationId a -> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      discardUntilIntoPlay you (CardWithType MinionType <> CardWithTrait T.Police)
      removeFromPlay a
      pure e
    _ -> Wanted <$> runMessage msg a
