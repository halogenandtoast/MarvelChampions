module Marvel.Obligation.Obligations.NextEvolutionTelepathySet where

import Marvel.Support.Types (Field (SupportController))
import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Resource
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.NextEvolution.Sinister

import Marvel.Card
import Marvel.Entity
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype TelepathySetObligation = TelepathySetObligation (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40160 :: ObligationCard TelepathySetObligation
ne40160 = obligation TelepathySetObligation Cards.ne40160

ne40161 :: ObligationCard TelepathySetObligation
ne40161 = obligation TelepathySetObligation Cards.ne40161

instance RunMessage TelepathySetObligation where
  runMessage msg card@(TelepathySetObligation a) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> do
      readyCards <- if toCardCode a == "40160"
        then map SupportRef <$> selectList (SupportMatches [UnexhaustedSupport, SupportControlledBy (IdentityWithId you)])
        else map AllyRef <$> selectList (UnexhaustedAlly <> AllyControlledBy (IdentityWithId you))
      for_ readyCards $ \case
        SupportRef sid -> exhaust sid
        AllyRef aid -> exhaust aid
        _ -> pure ()
      when (null readyCards) $ gainSurge a
      pure $ TelepathySetObligation a {obligationOwner = Just you}
    ObligationMessage oid (ResolvedObligation you) | oid == obligationId a -> do
      when (obligationSurge a) $ surge you
      pure card
    RanAbility _ (isTarget a -> True) 1 _ _ -> card <$ removeFromPlay a
    _ -> TelepathySetObligation <$> runMessage msg a

instance HasAbilities TelepathySetObligation where
  getAbilities card@(TelepathySetObligation a) = case obligationOwner a of
    Nothing -> []
    Just _ -> [ability card 1 AlterEgoAction OwnsThis
      (if toCardCode a == "40160" then ExhaustCost <> DiscardTopOfDeckForSupportsCost else MultiResourceCost [Just Mental, Just Mental])
      (runAbility card 1)]

instance HasModifiersFor TelepathySetObligation where
  getModifiersFor _ target (TelepathySetObligation a) = case obligationOwner a of
    Nothing -> pure []
    Just you -> case target of
      SupportRef sid | toCardCode a == "40160" -> do
        owned <- selectList $ SupportControlledBy (IdentityWithId you)
        let own = sid `elem` owned
        pure [CannotReady | own]
      AllyRef aid | toCardCode a == "40161" -> do
        own <- selectAny $ AllyWithId aid <> AllyControlledBy (IdentityWithId you)
        pure [CannotReady | own]
      _ -> pure []
