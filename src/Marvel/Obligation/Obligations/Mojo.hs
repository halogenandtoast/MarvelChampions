module Marvel.Obligation.Obligations.Mojo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

newtype PaparazziMeta = PaparazziMeta { paparazziThreat :: Natural }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MojoObligation = MojoObligation (Attrs Obligation `With` PaparazziMeta)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsObligation MojoObligation where
  toObligationAttrs (MojoObligation (a `With` _)) = a
  obligationDisplayThreat (MojoObligation (_ `With` meta)) = paparazziThreat meta

mojo39030 :: ObligationCard MojoObligation
mojo39030 = obligation (\a -> MojoObligation (a `With` PaparazziMeta 10)) Cards.mojo39030

instance HasAbilities MojoObligation where
  getAbilities (MojoObligation (a `With` meta)) = case obligationOwner a of
    Nothing -> []
    Just you ->
      (if paparazziThreat meta == 0 then [] else
        [ability a 1 Action (SelfMatches $ IdentityWithId you) (ExhaustCharacterCost $ CharacterControlledBy You) (runAbility a 1)
        ,ability a 2 Action (SelfMatches $ IdentityWithId you) (DiscardHandCardsCost 1 (Just 1)) (runAbility a 2)])
      <> [windowAbility a 3 (W.TurnEnds W.When $ IdentityWithId you) ForcedInterrupt NoCost (runAbility a 3)]

instance RunMessage MojoObligation where
  runMessage msg e@(MojoObligation (a `With` meta)) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility you (isTarget a -> True) idx _ _ | idx `elem` [1,2] -> do
      alterEgo <- identityMatches AlterEgoIdentity you
      let remaining = subtractNatural (if alterEgo then 3 else 2) (paparazziThreat meta)
      pure $ MojoObligation (a `With` PaparazziMeta remaining)
    RanAbility _ (isTarget a -> True) 3 _ _ -> do
      schemes <- selectList EncounterMainScheme
      traverse_ (`placeThreat` paparazziThreat meta) schemes
      removeFromPlay a
      pure e
    _ -> MojoObligation . (`With` meta) <$> runMessage msg a
