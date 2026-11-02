{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Obligation.Obligations.Fw54027 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Obligation.Types

import Marvel.Identity.Types (Field(PlayerIdentityHand))
fw54027 :: ObligationCard Fw54027
fw54027 = obligation Fw54027 Cards.fw54027

newtype Fw54027 = Fw54027 (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw54027 where
  runMessage msg a@(Fw54027 attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you [labeled "$label.doNotFlipToAlterEgo" (pure ()), labeled "$label.changeToB" (changeToAlterEgo you)]
      resolveObligation (obligationId attrs) you
      pure a
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      canExhaust <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      cards <- projectMap PlayerIdentityHand unHand you
      let cost :: PlayerCard -> Natural
          cost card = fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card
          highest = maximum (0 : map cost cards)
      chooseOne you $
        [labeled "$label.exhaustRemoveObligation" (exhaust you >> removeFromGame attrs) | canExhaust]
        <> [targeting card (discardCard card >> dealIndirectDamage you attrs (cost card)) | card <- cards, cost card == highest]
        <> [labeled "$label.discardObligation" (pure ()) | null cards]
      pure a
    _ -> Fw54027 <$> runMessage msg attrs
