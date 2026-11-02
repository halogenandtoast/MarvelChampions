module Marvel.CivilWar.Unregistered where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T

newtype UnregisteredSuper = UnregisteredSuper (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

unregisteredSuperDefinitions :: [CardDef]
unregisteredSuperDefinitions = [C.cw56106,C.cw56114]

unregisteredSupers :: [ObligationCard UnregisteredSuper]
unregisteredSupers = map (obligation UnregisteredSuper) unregisteredSuperDefinitions

instance HasModifiersFor UnregisteredSuper where
  getModifiersFor _ (IdentityRef you) (UnregisteredSuper a) | obligationOwner a == Just you = pure [TraitModifier T.Unregistered]
  getModifiersFor _ _ _ = pure []

instance HasAbilities UnregisteredSuper where
  getAbilities (UnregisteredSuper a) = [ability a 1 AlterEgoAction NoCriteria (DiscardMatchingHandCardCost HeroSpecificCard) (runAbility a 1) | isJust (obligationOwner a)]

instance RunMessage UnregisteredSuper where
  runMessage msg e@(UnregisteredSuper a) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> do
      hero <- identityMatches (IdentityWithId you <> HeroIdentity) you
      blanked <- isBlanked a
      unless blanked $ when hero $ cardSpecific GameRef "cwRevealedUnregistered" (CardId $ unObligationId oid,[you])
      UnregisteredSuper <$> runMessage msg a
    ObligationMessage oid ResolvedObligation{} | oid == obligationId a -> pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeFromPlay a
    _ -> UnregisteredSuper <$> runMessage msg a

newtype GovernmentContractor = GovernmentContractor (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

governmentContractor :: SideSchemeCard GovernmentContractor
governmentContractor = sideScheme GovernmentContractor C.cw56120 (Static 3)

instance HasModifiersFor GovernmentContractor where
  getModifiersFor _ (IdentityRef _) _ = pure [TraitModifier T.Unregistered]
  getModifiersFor _ _ _ = pure []

instance RunMessage GovernmentContractor where
  runMessage msg (GovernmentContractor a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      here <- getAreaOfTarget a
      players <- filterM (fmap (== here) . getAreaOfTarget) =<< getAllPlayers
      heroes <- filterM (\you -> identityMatches (IdentityWithId you <> HeroIdentity) you) players
      blanked <- isBlanked a
      unless blanked $ unless (null heroes) $ cardSpecific GameRef "cwRevealedUnregistered" (CardId $ unSideSchemeId sid,heroes)
      GovernmentContractor <$> runMessage msg a
    _ -> GovernmentContractor <$> runMessage msg a
