module Marvel.SideScheme.SideSchemes.NextEvolutionExtremeMeasures where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability
    ( AbilityType(ForcedResponse), windowAbility )
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Identity.Types
import Marvel.Keyword ()
import Marvel.Matchers hiding (UnexhaustedIdentity, UnexhaustedAlly)
import Marvel.Matchers qualified as Matchers ()
import Marvel.Modifier ( HasModifiersFor )
import Marvel.NextEvolution.Modulars ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question
import Marvel.Resource.Types ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types


import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype ExtremeMeasuresSideScheme = ExtremeMeasuresSideScheme (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40184 :: SideSchemeCard ExtremeMeasuresSideScheme
ne40184 = sideScheme ExtremeMeasuresSideScheme Cards.ne40184 (Static 3)


instance IsSideScheme ExtremeMeasuresSideScheme where
  sideSchemeAbilities a = [windowAbility a 1 (W.PlayerCardEntersPlay W.After AnyIdentity AnyCard)
    ForcedResponse NoCost (runAbility a 1)]

instance RunMessage ExtremeMeasuresSideScheme where
  runMessage msg a@(ExtremeMeasuresSideScheme attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(you,card) | W.PlayerCardEnteredPlay you card <- windows] $ \(you,card) -> do
        let cost = fromIntegral $ max 0 $ fromMaybe 0 $ cdCost $ getCardDef card
        when (cost > 0) $ dealIndirectDamage you attrs cost
      pure a
    _ -> ExtremeMeasuresSideScheme <$> runMessage msg attrs
