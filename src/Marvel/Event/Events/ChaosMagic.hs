module Marvel.Event.Events.ChaosMagic (
  chaosMagic,
  chaosMagicEffect,
  ChaosMagic (..),
  ChaosMagicEffect (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref

chaosMagic :: EventCard ChaosMagic
chaosMagic = event ChaosMagic Cards.chaosMagic

newtype ChaosMagic = ChaosMagic (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ignoring the cost is an effect on the player, gone once they play a card; which
-- cards may be played is asked once it is there, so any card in hand that could be
-- played for nothing is offered. Then as many encounter cards as the card's printed
-- cost are discarded.
instance RunMessage ChaosMagic where
  runMessage msg e@(ChaosMagic attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      createEffect Cards.chaosMagic attrs (IdentityEntity $ IdentityWithId you)
      cardSpecific attrs "play" you
      pure e
    CardSpecific (isTarget attrs -> True) "play" (fromJSON -> Success you) -> do
      cards <- selectList $ InHandOf (IdentityWithId you) $ AffordableCardBy (IdentityWithId you) (BasicCardMatches AnyCard)
      chooseOne
        you
        [ do
            discards <- capture $ discardTopOfEncounterDeck (fromIntegral . max 0 . fromMaybe 0 . cdCost $ getCardDef c)
            pure $ TargetLabel (toRef c) [PlayCard c Nothing, Run discards]
        | c <- cards
        ]
      -- the effect ends with the card played, or now with none to play
      cardSpecific attrs "done" ()
      pure e
    _ -> ChaosMagic <$> runMessage msg attrs

newtype ChaosMagicEffect = ChaosMagicEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

chaosMagicEffect :: CardEffect ChaosMagicEffect
chaosMagicEffect =
  effectWith ChaosMagicEffect Cards.chaosMagic $
    (modifiersL .~ [IgnoresResourceCost]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage ChaosMagicEffect where
  runMessage msg e@(ChaosMagicEffect attrs) = runQueueT $ case msg of
    IdentityMessage ident (PlayedCard _ _) -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    CardSpecific target "done" _ | target == refocus (effectSource attrs) -> do
      disableEffect (effectId attrs)
      pure e
    _ -> ChaosMagicEffect <$> runMessage msg attrs
