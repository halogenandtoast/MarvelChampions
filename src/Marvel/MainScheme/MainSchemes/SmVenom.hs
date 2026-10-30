module Marvel.MainScheme.MainSchemes.SmVenom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Id
import Marvel.Cost
import Marvel.Entity
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Environment.Cards qualified as Environments
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window
import Marvel.Window qualified as W

newtype SmVenomMainScheme = SmVenomMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsMainScheme SmVenomMainScheme where
  mainSchemeAbilities (SmVenomMainScheme a) =
    [windowAbility a 1 (EnemyActivates When VillainEnemy AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]

smLeaveUsAlone :: MainSchemeCard SmVenomMainScheme
smLeaveUsAlone = mainScheme SmVenomMainScheme Cards.smLeaveUsAlone (PerPlayer 10) (PerPlayer 2) (PerPlayer 1)

instance RunMessage SmVenomMainScheme where
  runMessage msg e@(SmVenomMainScheme attrs) = runQueueT $ case msg of
    AdvanceMainScheme sid | sid == mainSchemeId attrs -> e <$ gameOver Lost
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      putEncounterCardIntoPlay first =<< genEncounterCard Environments.smBellTowerA
      pure e
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [(vid, you) | W.EnemyActivation (EnemyVillainId vid) you <- windows] \(vid, you) ->
        identityStep you (MoveIdentityBoostCardsToVillain vid)
      pure e
    _ -> SmVenomMainScheme <$> runMessage msg attrs
