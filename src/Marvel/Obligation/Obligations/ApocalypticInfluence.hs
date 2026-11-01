module Marvel.Obligation.Obligations.ApocalypticInfluence where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Side
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

apocalypticInfluence :: ObligationCard ApocalypticInfluence
apocalypticInfluence = obligation ApocalypticInfluence Cards.apocalypticInfluence

newtype ApocalypticInfluence = ApocalypticInfluence (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ApocalypticInfluence where
  getAbilities a@(ApocalypticInfluence attrs) =
    [ ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId you)
        (DealEncounterCardToPlayerCost FirstPlayer) (runAbility a 1)
    | Just you <- [obligationOwner attrs] ]

instance RunMessage ApocalypticInfluence where
  runMessage msg a@(ApocalypticInfluence attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation revealedFor) | oid == obligationId attrs -> do
      owner <- selectOne $ IdentityIs Heroes.angelHero
      let you = fromMaybe revealedFor owner
      archangel <- identityMatches (IdentityWithTitle "Archangel") you
      if archangel
        then do
          scheme <- asYou you $ selectOne EncounterMainScheme
          for_ scheme $ \sid -> placeThreat sid 2
        else changeToForm you C
      pure $ ApocalypticInfluence attrs {obligationOwner = Just you}
    -- This obligation stays in its owner's play area until its action discards it.
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromPlay attrs
    _ -> ApocalypticInfluence <$> runMessage msg attrs
