module Marvel.SideScheme.SideSchemes.NextEvolutionNeStryfe where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.NextEvolution.Stryfe
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Id
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NeStryfeSideScheme = NeStryfeSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40168a :: SideSchemeCard NeStryfeSideScheme
ne40168a = sideScheme (\a -> NeStryfeSideScheme a {sideSchemeCrisis = True}) Cards.ne40168a (Static 4)

ne40168b :: SideSchemeCard NeStryfeSideScheme
ne40168b = sideScheme (\a -> NeStryfeSideScheme a {sideSchemeCrisis = True}) Cards.ne40168b (Static 3)

ne40175 :: SideSchemeCard NeStryfeSideScheme
ne40175 = sideScheme NeStryfeSideScheme Cards.ne40175 (PerPlayer 2)

ne40176 :: SideSchemeCard NeStryfeSideScheme
ne40176 = sideScheme NeStryfeSideScheme Cards.ne40176 (Static 2)

instance IsSideScheme NeStryfeSideScheme where
  sideSchemeAbilities (NeStryfeSideScheme a) =
    [windowAbility a 1 (W.EnemyDefeated W.After (EnemyWithTitle "Stryfe") AnyDamage) ForcedResponse NoCost (runAbility a 1) | toCardCode a == "40168a"]

instance HasModifiersFor NeStryfeSideScheme where
  getModifiersFor _ target (NeStryfeSideScheme a) = case toCardCode a of
    "40168a" | target == toRef a -> pure [CannotLeavePlay]
    "40168a" | AllyRef aid <- target -> do
      hope <- selectAny (AllyWithId aid <> AllyWithTitle "Hope Summers")
      if not hope then pure [] else do
        enemies <- selectList (NotEnemy $ EnemyWithTitle "Stryfe")
        main <- selectList MainScheme
        sides <- selectList (NotSideScheme $ SideSchemeWithId $ sideSchemeId a)
        pure $ map CannotAttackEnemy enemies <> map CannotThwartScheme main <> map (CannotThwartScheme . SchemeSideSchemeId) sides
    "40168b" | VillainRef _ <- target -> pure [CannotBeDefeated]
    _ -> pure []

instance RunMessage NeStryfeSideScheme where
  runMessage msg e@(NeStryfeSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      case toCardCode a of
        "40168b" -> selectJust MainScheme >>= \case
          SchemeMainSchemeId mid -> nextMainScheme mid
          _ -> pure ()
        "40175" -> selectJust You >>= returnControlledCard
        "40176" -> do
          players <- selectList AnyIdentity
          for_ players $ \you -> do
            n <- handTypeCount you
            placeThreat (sideSchemeId a) n
        _ -> pure ()
      NeStryfeSideScheme <$> runMessage msg a
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a, toCardCode a == "40175" -> do
      you <- getDefeatedBy a
      returnControlledCard you
      NeStryfeSideScheme <$> runMessage msg a
    RanAbility _ (isTarget a -> True) 1 _ _ | toCardCode a == "40168a" -> do
      cardSpecific a "neFlipLivingBomb" ()
      pure e
    CardSpecific target "neFlipLivingBomb" _ | target == toRef a, toCardCode a == "40168a" -> do
      -- Revealing the reverse places its printed initial threat, then transfers the
      -- threat remaining on the front. The physical scheme never leaves play.
      checkWindows [W.Window W.After $ W.SideSchemeEnteredPlay $ sideSchemeId a]
      sideSchemeStep (sideSchemeId a) RevealSideScheme
      pure $ NeStryfeSideScheme a {sideSchemeCardDef = Cards.ne40168b, sideSchemeInitialThreat = Static 3, sideSchemeThreat = sideSchemeThreat a + 3}
    CardSpecific target "moveThreatOut" (fromJSON -> Success n) | target == toRef a, toCardCode a == "40168a" -> do
      let remaining = subtractNatural n (sideSchemeThreat a)
      when (sideSchemeThreat a > 0 && remaining == 0) $ cardSpecific a "neFlipLivingBomb" ()
      pure $ NeStryfeSideScheme a {sideSchemeThreat = remaining}
    _ -> do
      attrs <- runMessage msg a
      when (toCardCode a == "40168a" && sideSchemeThreat a > 0 && sideSchemeThreat attrs == 0) $ cardSpecific a "neFlipLivingBomb" ()
      pure $ NeStryfeSideScheme attrs
