module Marvel.Obligation.Obligations.RegenerationCycle (
  regenerationCycle,
  RegenerationCycle (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Aspect
import Marvel.Card
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ref

regenerationCycle :: ObligationCard RegenerationCycle
regenerationCycle = obligation RegenerationCycle Cards.regenerationCycle

newtype RegenerationCycle = RegenerationCycle (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego. Choose: exhaust your alter-ego → remove this card from
-- the game (only as a ready alter-ego, R20); or discard the top 5 cards of your deck,
-- place 1 threat on the main scheme for each different aspect discarded, and discard
-- this obligation.
instance RunMessage RegenerationCycle where
  runMessage msg o@(RegenerationCycle attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation you -> do
        isHero <- identityMatches HeroIdentity you
        when isHero $
          chooseOne you
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo you)
            ]
        resolveObligation (obligationId attrs) you
        pure o
      ResolveObligation you -> do
        canExhaust <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
        chooseOrRunOne you $
          [labeled "$label.exhaustAdamWarlockRemoveRegenerationCycle" (exhaust you >> removeFromGame attrs) | canExhaust]
            <> [labeled "$label.discardTopFiveThreatPerAspect" (discardFromDeck you 5 attrs)]
        pure o
      _ -> RegenerationCycle <$> runMessage msg attrs
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      let x = distinctAspects $ map (cdAspect . getCardDef) cards
      when (x > 0) $ placeThreatOnEach attrs x EncounterMainScheme
      pure o
    _ -> RegenerationCycle <$> runMessage msg attrs
