module Marvel.AgeOfApocalypse.Settings where

import Marvel.Prelude
import Marvel.Card
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

currentSetting :: HasGame m => m (Maybe (EnvironmentId, CardDef))
currentSetting = do
  settings <- selectList (EnvironmentWithTrait Trait.Setting)
  game <- getGame
  pure $ headMay [(eid, def) | eid <- settings, Just def <- [abilityCardDef game $ EnvironmentRef eid]]

-- | The named Special belongs to the current setting, including when that
-- modular is used by itself in a different scenario.
resolveSettingSpecial :: ReverseQueue m => IdentityId -> m ()
resolveSettingSpecial you = resolveSettingSpecialFor you GameRef

-- | The Savage Land's actual discarded cards are reported after discard
-- windows, rather than predicting its icons from cards still in the deck.
resolveSettingSpecialFor :: (ReverseQueue m, IsRef target) => IdentityId -> target -> m ()
resolveSettingSpecialFor you target = do
  setting <- currentSetting
  active <- case setting of
    Just (eid, _) -> not <$> isBlanked eid
    Nothing -> pure False
  case setting of
    Just (_, def) | active, toCardCode def == "45127" -> discardFromDeck you 3 target
    Just (eid, def) | active, toCardCode def == "45133" -> do
      placeThreatOnEach eid 1 MainScheme
      withDiscarded target FromDeck []
    Just (eid, def) | active, toCardCode def == "45139" -> do
      damageIdentity you eid (toDamage 1 FromAbility)
      withDiscarded target FromDeck []
    _ -> withDiscarded target FromDeck []

replaceOtherSettings :: ReverseQueue m => EnvironmentId -> m ()
replaceOtherSettings current = do
  settings <- selectList (EnvironmentWithTrait Trait.Setting)
  traverse_ removeFromPlay (filter (/= current) settings)
