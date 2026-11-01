module Marvel.SideScheme.SideSchemes.KeepUpThePressure where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Effect.Types
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Deck
import Marvel.Discard
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityDiscard))
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.SideScheme.Types

keepUpThePressure :: SideSchemeCard KeepUpThePressure
keepUpThePressure = sideScheme KeepUpThePressure Cards.keepUpThePressure (PerPlayer 2)

newtype KeepUpThePressure = KeepUpThePressure (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage KeepUpThePressure where
  runMessage msg a@(KeepUpThePressure attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players $ cardSpecific attrs "search"
      createEffect Cards.keepUpThePressure attrs (EventsMatching (CardWithType EventType <> CardWithTrait Trait.Attack))
      pure a
    CardSpecific (isTarget attrs -> True) "search" (fromJSON -> Success you) -> do
      deck <- projectMap PlayerIdentityDeck unDeck you
      discard <- projectMap PlayerIdentityDiscard unDiscard you
      let cards = filter (cardMatch $ CardWithType EventType <> CardWithTrait Trait.Attack) (deck <> discard)
      focusing (map PlayerCard cards) $ chooseOne you $ labeled "$label.noResponse" (pure ()) : [targeting card (addToHand you card) | card <- cards]
      shuffleDeck you
      pure a
    _ -> KeepUpThePressure <$> runMessage msg attrs
newtype KeepUpThePressureEffect = KeepUpThePressureEffect (Attrs Effect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
instance IsEffect KeepUpThePressureEffect where
  getEffectModifiers _ = pure [DamageDealtModifier 1]
keepUpThePressureEffect :: CardEffect KeepUpThePressureEffect
keepUpThePressureEffect = effectWith KeepUpThePressureEffect Cards.keepUpThePressure (endsL ?~ DisableAtEndOfPhase)
instance RunMessage KeepUpThePressureEffect where
  runMessage msg (KeepUpThePressureEffect attrs) = runQueueT $ KeepUpThePressureEffect <$> runMessage msg attrs
