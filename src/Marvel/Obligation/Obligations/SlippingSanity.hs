module Marvel.Obligation.Obligations.SlippingSanity (
  slippingSanity,
  SlippingSanity (..),
) where

import Marvel.Prelude

import Marvel.Ability.Types (HasAbilities)
import Marvel.Boost (BoostIcon (Star))
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query

slippingSanity :: ObligationCard SlippingSanity
slippingSanity = obligation SlippingSanity Cards.slippingSanity

newtype SlippingSanity = SlippingSanity (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | You may flip to alter-ego form. Choose: exhaust Wanda Maximoff → remove Slipping
-- Sanity from the game (only while the identity is Wanda Maximoff and ready: RR Target,
-- Choose (Option)); or discard the top 5 encounter cards, 1 threat on the main scheme for
-- each star icon among them (stars are not boost icons), and discard this obligation.
instance RunMessage SlippingSanity where
  runMessage msg o@(SlippingSanity attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        isHero <- identityMatches (HeroIdentity <> IdentityCanChangeForm) identityId
        when isHero $
          chooseOne identityId
            [ labeled "$label.doNotFlipToAlterEgo" (pure ())
            , labeled "$label.changeToB" (changeToAlterEgo identityId)
            ]
        resolveObligation (obligationId attrs) identityId
        SlippingSanity <$> runMessage msg attrs
      ResolveObligation identityId -> do
        readyWanda <- identityMatches (IdentityWithTitle "Wanda Maximoff" <> UnexhaustedIdentity) identityId
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustWandaMaximoffRemoveSlippingSanity" (exhaust identityId >> removeFromGame attrs)
          | readyWanda
          ]
            <> [ labeled (countLabel "discardEncounterCardsThreatPerStar" 5) $
                  discardTopOfEncounterDeckThen 5 \cards -> do
                    let stars = count (== Star) $ concatMap (cdBoostIcons . getCardDef) cards
                    mainScheme <- selectOne MainScheme
                    for_ mainScheme \scheme -> placeThreat scheme stars
               ]
        pure o
      _ -> SlippingSanity <$> runMessage msg attrs
    _ -> SlippingSanity <$> runMessage msg attrs
