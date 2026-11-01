module Marvel.Ally.Allies.Nc48021 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost qualified as BoostIcon
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Keyword ()
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Runner
import Marvel.Ally.Cards qualified as Cards

nc48021 :: AllyCard Nc48021
nc48021 = ally Nc48021 Cards.nc48021 (Thw 0, 1) (Atk 0, 1) (HP 3)

newtype Nc48021 = Nc48021 (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Nc48021 where
  getModifiersFor _ (AllyRef aid) (Nc48021 attrs) | aid == allyId attrs = do
    let n = fromIntegral $ length [() | card <- allyHeldTreacheries attrs, icon <- cdBoostIcons (getCardDef card), icon == BoostIcon.Boost]
    pure [AttackModifier n, ThwartModifier n]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Nc48021 where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Nc48021 where
  runMessage msg a@(Nc48021 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ lookAtTopOfEncounterDeck 3 attrs
    SearchFoundCards (isTarget attrs -> True) (onlyEncounterCards -> cards) -> do
      chooseOne (allyController attrs) [targeting card do
        cardSpecific attrs "tuck" card
        putOnTopOfEncounterDeck [other | other <- cards, other /= card]
        | card <- cards]
      pure a
    CardSpecific (isTarget attrs -> True) "tuck" (fromJSON -> Success card) ->
      pure $ Nc48021 attrs {allyHeldTreacheries = [card]}
    _ -> Nc48021 <$> runMessage msg attrs
