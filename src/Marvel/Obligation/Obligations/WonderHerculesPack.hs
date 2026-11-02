module Marvel.Obligation.Obligations.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.WonderMan.Helpers
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC

wm58025 :: ObligationCard WmHcObligation
wm58025 = obligation (WmHcObligation . (`With` (3, []))) WM.wm58025

hc59004 :: ObligationCard WmHcObligation
hc59004 = obligation (WmHcObligation . (`With` (3, []))) HC.hc59004

hc59035 :: ObligationCard WmHcObligation
hc59035 = obligation (WmHcObligation . (`With` (3, []))) HC.hc59035

data LaborAttack = LaborAttack {laborEnemy :: EnemyId, laborTracked :: Bool, laborDefended :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WmHcObligation = WmHcObligation (Attrs Obligation `With` (Natural, [LaborAttack]))
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsObligation WmHcObligation where
  toObligationAttrs (WmHcObligation (attrs `With` _)) = attrs
  obligationDisplayCounters (WmHcObligation (attrs `With` (n, _))) = if toCardCode attrs == "59004" then n else 0

instance HasModifiersFor WmHcObligation where
  getModifiersFor _ (IdentityRef you) (WmHcObligation (attrs `With` _)) | obligationOwner attrs == Just you && toCardCode attrs == "58025" = pure [CannotAttack]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WmHcObligation where
  getAbilities o@(WmHcObligation (attrs `With` _)) = case obligationOwner attrs of
    Nothing -> []
    Just you -> case toCardCode attrs of
      "58025" -> [ability o 1 AlterEgoAction owner ExhaustCost $ runAbility o 1
                 ,ability o 2 AlterEgoAction (owner <> UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (AtLeast $ Static 3)) NoCost $ runAbility o 2]
      "59035" -> [ability o 1 AlterEgoAction owner ExhaustCost $ runAbility o 1
                 ,ability o 2 AlterEgoAction owner (MultiResourceCost [Just Mental, Just Mental]) $ runAbility o 1]
      "59004" -> [limitedWindowAbility o 1 (W.EnemyAttacksPlayerOrAlly W.When VillainEnemy (IdentityWithId you)) ForcedInterrupt (SelfMatches (IdentityWithId you) <> AllyExists (AllyControlledBy $ IdentityWithId you)) NoCost $ runAbility o 1]
      _ -> []
     where owner = SelfMatches $ IdentityWithId you

instance RunMessage WmHcObligation where
  runMessage msg o@(WmHcObligation (attrs `With` (counters, watching))) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      when (toCardCode attrs == "59004") do
        present <- selectAny $ AllyIs HC.hc59008
        unless present $ findOwnedPlayerCard you "59008" >>= traverse_ (\card -> removeFromGame card >> putIntoPlay you card)
      pure $ WmHcObligation (attrs {obligationOwner = Just you} `With` (counters, watching))
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) idx windows _ -> case toCardCode attrs of
      "59004" -> do
        allies <- selectList $ AllyControlledBy $ IdentityWithId you
        let enemies = [enemy | W.EnemyAttack enemy who <- windows, who == you]
        for_ enemies $ \enemy -> chooseOne you [targeting aid $ redirectEnemyAttackToAlly enemy you aid | aid <- allies]
        pure $ WmHcObligation (attrs `With` (counters, foldr (markLaborAttack (\frame -> frame {laborTracked = True})) watching enemies))
      "58025" -> do
        when (idx == 2) do
          cards <- ionicCards you
          host <- ionicPhysiologyFor you
          for_ host $ \uid -> traverse_ (discardHeldPlayerCard you uid) $ take 3 cards
        removeFromPlay attrs
        pure o
      "59035" -> o <$ removeFromGame attrs
      _ -> pure o
    CheckWindows windows | toCardCode attrs == "59004", Just you <- obligationOwner attrs -> do
      let starts = [enemy | W.Window W.When (W.EnemyAttack enemy _) <- windows, isVillain enemy]
            <> [enemy | W.Window W.When (W.EnemyAttackAlly enemy _ _) <- windows, isVillain enemy]
          opened = foldr (\enemy frames -> LaborAttack enemy False False : frames) watching starts
          defended = foldr (markLaborAttack (\frame -> frame {laborDefended = True})) opened [enemy | W.Window W.After (W.HeroDefends who enemy) <- windows, who == you]
          finishes = [enemy | W.Window W.After (W.EnemyAttack enemy _) <- windows]
            <> [enemy | W.Window W.After (W.EnemyAttackAlly enemy _ _) <- windows]
          (frames, earned) = foldl closeLaborAttack (defended, 0) finishes
          remaining = subtractNatural earned counters
      when (earned > 0 && remaining == 0) $ addToVictoryDisplay attrs
      pure $ WmHcObligation (attrs `With` (remaining, frames))
    _ -> (\attrs' -> WmHcObligation (attrs' `With` (counters, watching))) <$> runMessage msg attrs

isVillain :: EnemyId -> Bool
isVillain (EnemyVillainId _) = True
isVillain _ = False

-- Each declaration has a frame, including attacks not redirected by this Labor.
-- This keeps nested attacks by the same villain distinct without persistent IDs.
markLaborAttack :: (LaborAttack -> LaborAttack) -> EnemyId -> [LaborAttack] -> [LaborAttack]
markLaborAttack _ _ [] = []
markLaborAttack f enemy (frame : rest)
  | laborEnemy frame == enemy = f frame : rest
  | otherwise = frame : markLaborAttack f enemy rest

closeLaborAttack :: ([LaborAttack], Natural) -> EnemyId -> ([LaborAttack], Natural)
closeLaborAttack ([], earned) _ = ([], earned)
closeLaborAttack (frame : rest, earned) enemy
  | laborEnemy frame == enemy = (rest, earned + if laborTracked frame && laborDefended frame then 1 else 0)
  | otherwise = let (rest', earned') = closeLaborAttack (rest, earned) enemy in (frame : rest', earned')
