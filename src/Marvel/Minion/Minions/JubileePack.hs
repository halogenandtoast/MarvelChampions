module Marvel.Minion.Minions.JubileePack where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Game.Source (getsGame, abilityCardDef)
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Minion.Runner
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype JubileeMinion = JubileeMinion (Attrs Minion)
  deriving anyclass IsMinion
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
jubilee47024, jubilee47030 :: MinionCard JubileeMinion
jubilee47024 = minion JubileeMinion Cards.jubilee47024 (Sch 2) (Atk 1) (HP 4)
jubilee47030 = minion JubileeMinion Cards.jubilee47030 (Sch 2) (Atk 2) (HP 3)
instance HasModifiersFor JubileeMinion where
  getModifiersFor _ (MinionRef mid) (JubileeMinion attrs) | mid == minionId attrs, toCardCode attrs == "47030" = do
    trap <- anyM (\sid -> getsGame (maybe False (cardMatch $ CardWithTrait T.Trap) . (`abilityCardDef` SideSchemeRef sid))) =<< selectList AnySideScheme
    pure [CannotTakeDamage | trap]
  getModifiersFor _ _ _ = pure []
instance HasAbilities JubileeMinion where
  getAbilities a@(JubileeMinion attrs) | toCardCode attrs == "47024" =
    [limitedWindowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) You)
      ForcedResponse (AllyExists $ AllyControlledBy You) NoCost (runAbility a 1)]
  getAbilities _ = []
instance RunMessage JubileeMinion where
  runMessage msg a@(JubileeMinion attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion _) | mid == minionId attrs, toCardCode attrs == "47030" -> do
      discardUntil FromEncounterDeck (CardWithTrait T.Trap <> CardWithType SideSchemeType) attrs
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      revealEncounterCard (minionEngagedIdentity attrs) card
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      let attacked = [you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows]
      for_ (headMay attacked) $ \you -> whenM (selectAny $ AllyControlledBy $ IdentityWithId you) $
        searchEverywhereAndRevealEncounterCard you (CardIs Cards.jubilee47027)
      pure a
    _ -> JubileeMinion <$> runMessage msg attrs
