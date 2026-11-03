module Marvel.CivilWar.RoyalGuard where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Atlanteans qualified as Shared
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.Obligation.Types
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

newtype RoyalGuard = RoyalGuard (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

royalGuard :: [MinionCard RoyalGuard]
royalGuard = map make [C.cw57069,C.cw57070]
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
    in minion RoyalGuard def (Sch sch) (Atk atk) (HP hp)

instance RunMessage RoyalGuard where
  runMessage msg e@(RoyalGuard a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      hunted <- asYou you $ identityMatches (IdentityWithTrait T.Hunted) you
      allowed <- notElem CannotActivate <$> getModifiers a
      if hunted && allowed then minionActivatesAgainst mid you else discardTopOfDeck you 2
      pure e
    _ -> RoyalGuard <$> runMessage msg a

atlanteans :: TreacheryCard Shared.Atlanteans
atlanteans = treachery Shared.Atlanteans C.cw57071

newtype BloodDebt = BloodDebt (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

bloodDebt :: ObligationCard BloodDebt
bloodDebt = obligation BloodDebt C.cw57072

instance HasModifiersFor BloodDebt where
  getModifiersFor _ (IdentityRef you) (BloodDebt a) | obligationOwner a == Just you = pure [TraitModifier T.Hunted]
  getModifiersFor _ _ _ = pure []

instance HasAbilities BloodDebt where
  getAbilities (BloodDebt a) =
    [ability a 1 AlterEgoAction NoCriteria (DiscardTopOfDeckCardsCost 8 8) (runAbility a 1)
    | isJust (obligationOwner a)]

instance RunMessage BloodDebt where
  runMessage msg e@(BloodDebt a) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeFromPlay a
    _ -> BloodDebt <$> runMessage msg a

newtype TheRoyalGuard = TheRoyalGuard (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

theRoyalGuard :: SideSchemeCard TheRoyalGuard
theRoyalGuard = sideScheme TheRoyalGuard C.cw57073 (Static 3)

instance RunMessage TheRoyalGuard where
  runMessage msg (TheRoyalGuard a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- getDefeatedBy a
      deck <- asYou you getEncounterDeck
      discard <- asYou you getEncounterDiscard
      key <- getDeckKeyFor you
      let cards = filter (cardMatch $ CardIs C.cw57072) (deck <> discard)
      lookForEnemyTeam you a cards [cardLabeled (toCardCode card) $ dealFoundEncounterCard you card | card <- cards]
      fromDeck key shuffleEncounterDeck
      TheRoyalGuard <$> runMessage msg a
    _ -> TheRoyalGuard <$> runMessage msg a
