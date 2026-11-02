module Marvel.Resource.Resources.Shield where
import Marvel.Prelude
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Resource.Cards qualified as Cards
import Marvel.Trait qualified as T
import Marvel.Window qualified as W

specialFundingSpent :: ReverseQueue m => IdentityId -> PlayerCard -> PlayerCard -> m ()
specialFundingSpent you played spent = when (canonicalCardCode (getCardDef spent) == "50007" && cardMatch (CardWithType SupportType <> CardWithTrait T.Shield) played) $
  chooseOne you
    [labeled "$label.specialFunding" $ createEffect Cards.aosP50007 spent (SourceEntity $ CardIdRef $ pcCardId played)
    ,labeled "$label.noResponse" (pure ())]

newtype SpecialFundingEffect = SpecialFundingEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show,Eq,ToJSON,FromJSON,IsRef)

specialFundingEffect :: CardEffect SpecialFundingEffect
specialFundingEffect = effect SpecialFundingEffect Cards.aosP50007

instance RunMessage SpecialFundingEffect where
  runMessage msg e@(SpecialFundingEffect attrs) = runQueueT $ case msg of
    CheckWindows windows -> do
      for_ [card | W.Window W.After (W.PlayerCardEnteredPlay _ card) <- windows, effectMatcher attrs == SourceEntity (CardIdRef $ pcCardId card)] $ \card -> do
        placeCounters (SupportId $ unCardId $ pcCardId card) 1
        disableEffect (effectId attrs)
      pure e
    _ -> SpecialFundingEffect <$> runMessage msg attrs
