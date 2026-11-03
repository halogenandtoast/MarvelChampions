module Marvel.Shadowland.NinjaSwarm where

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
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Types
import Marvel.Minion.Runner ()
import Marvel.Modifier
import Marvel.Question
import Marvel.Query
import Marvel.Ref
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Stats
import Marvel.Treachery.Types
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

newtype Ninja = Ninja (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsMinion

ninjas :: [MinionCard Ninja]
ninjas = [minion Ninja def (Sch sch) (Atk atk) (HP hp) | (def,sch,atk,hp) <-
  [(Cards.street65028,2,2,5),(Cards.street65029,1,2,3),(Cards.street65030,1,1,2),
   (Cards.street65031,1,1,2),(Cards.street65032,1,1,2),(Cards.street65033,1,1,2),(Cards.street65034,1,1,2)]]

instance HasModifiersFor Ninja where
  getModifiersFor _ target (Ninja a) = case target of
    MinionRef mid | toCardCode a == "65028", mid /= minionId a -> do
      affected <- minionMatches (MinionWithId mid <> MinionWithTrait T.Ninja <> MinionEngagedWith (IdentityWithId $ minionEngagedIdentity a)) mid
      pure [CannotBeAttacked | affected]
    _ | target == toRef a -> pure $ case toCardCode a of
      "65031" -> [KeywordModifier Ranged]
      "65034" -> [KeywordModifier Piercing]
      _ -> []
    _ -> pure []

instance HasAbilities Ninja where
  getAbilities (Ninja a) =
    [windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId $ minionId a) WhenRevealed NoCost (runAbility a 1) | toCardCode a /= "65028"]

instance RunMessage Ninja where
  runMessage msg e@(Ninja a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      who <- maybe (pure you) (fmap (fromMaybe you) . getSourceController) $ headMay [s | W.DefeatedMinion _ _ s _ <- windows]
      if toCardCode a == "65029" then do
        canPay <- canAffordCost who (toSource a) (MultiResourceCost [Nothing,Nothing])
        chooseOne who $ [labeled "$label.slPayTwo" (payForTreachery who $ MultiResourceCost [Nothing,Nothing]) | canPay]
          <> [labeled "$label.slShuffleKirigi" $ afterMatching (== MinionMessage (minionId a) MinionDefeated) $ shuffleMinionIntoEncounterDeck (minionId a)]
      else discardTopOfEncounterDeckThen 2 $ \cards ->
        for_ (headMay $ filter (\c -> cdCardType (getCardDef c) == MinionType && any (\case Teamwork _ -> True; _ -> False) (cdKeywords $ getCardDef c)) cards) $ revealFoundEncounterCard who
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      attack <- getEnemyAttack enemy
      case attack of
        Just atk | attackDefended atk, AllyCharacter _ <- attackCharacter atk -> do
          you <- getBoostedPlayer enemy
          if toCardCode a == "65028" then revealBoost a you
          else when (toCardCode a == "65029") $ addEnemyBoost enemy 2 >> attackGains enemy Overkill
        _ -> pure ()
      pure e
    _ -> Ninja <$> runMessage msg a

newtype DancingShadows = DancingShadows (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance IsSideScheme DancingShadows

dancingShadows :: SideSchemeCard DancingShadows
dancingShadows = sideScheme DancingShadows Cards.street65035 (Static 3)

instance HasModifiersFor DancingShadows where
  getModifiersFor _ target (DancingShadows a) = case target of
    MinionRef mid -> do
      ninja <- minionMatches (MinionWithTrait T.Ninja) mid
      pure [HitPointModifier 1 | ninja]
    _ | target == toRef a -> do
      ninja <- selectAny (MinionWithTrait T.Ninja)
      pure [ThreatCannotBeRemoved | ninja]
    _ -> pure []
instance RunMessage DancingShadows where
  runMessage msg e@(DancingShadows a) = runQueueT $ case msg of
    Boost (RevealedAsBoost target enemy) | isTarget a target -> getBoostedPlayer enemy >>= revealBoost a >> pure e
    _ -> DancingShadows <$> runMessage msg a

newtype CountlessNumbers = CountlessNumbers (Attrs Treachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsTreachery
countlessNumbers :: TreacheryCard CountlessNumbers
countlessNumbers = treachery CountlessNumbers Cards.street65036
instance RunMessage CountlessNumbers where
  runMessage msg e@(CountlessNumbers a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery _) | tid == treacheryId a -> reshuffle >> pure e
    Boost (RevealedAsBoost target _) | isTarget a target -> reshuffle >> pure e
    _ -> CountlessNumbers <$> runMessage msg a
   where
    reshuffle = getEncounterDiscard >>= shuffleIntoEncounterDeck . filter (cardMatch $ CardWithType MinionType <> CardWithTrait T.Ninja)
