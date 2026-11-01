module Marvel.MainScheme.MainSchemes.SmVenomGoblin where

import Marvel.Prelude
import Marvel.Ability.Types ()
import Data.Aeson ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers hiding (MainScheme)
import Marvel.Matchers qualified as Matchers
import Marvel.Count (selectCount)
import Marvel.Query
import Marvel.Environment.Cards qualified as Environments
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp ()
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.Stats ()
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types

newtype SmVenomGoblinMainScheme = SmVenomGoblinMainScheme (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSkiesOverNewYork :: MainSchemeCard SmVenomGoblinMainScheme
smSkiesOverNewYork = mainScheme SmVenomGoblinMainScheme Cards.smSkiesOverNewYork (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

smLowerManhattan :: MainSchemeCard SmVenomGoblinMainScheme
smLowerManhattan = mainScheme SmVenomGoblinMainScheme Cards.smLowerManhattan (PerPlayer 11) (PerPlayer 1) (PerPlayer 1)

smMidtownManhattan :: MainSchemeCard SmVenomGoblinMainScheme
smMidtownManhattan = mainScheme SmVenomGoblinMainScheme Cards.smMidtownManhattan (PerPlayer 12) (PerPlayer 2) (PerPlayer 1)

smUpperManhattan :: MainSchemeCard SmVenomGoblinMainScheme
smUpperManhattan = mainScheme SmVenomGoblinMainScheme Cards.smUpperManhattan (PerPlayer 10) (Static 0) (PerPlayer 1)

-- Festering Mass counts as an environment only while no actual symbiote
-- environment is in play; its presence therefore contributes exactly one.
symbioteEnvironmentCount :: (HasGame m) => m Natural
symbioteEnvironmentCount = do
  environments <- selectList AnyEnvironment
  real <- filterM (\eid -> getsGame $ maybe False ((`elem` ["27117b", "27118b", "27119b"]) . toCardCode) . (`abilityCardDef` EnvironmentRef eid)) environments
  mass <- selectAny (SideSchemeIs SideSchemes.smFesteringMass)
  pure $ if null real && mass then 1 else fromIntegral (length real)

moveGlider :: (ReverseQueue m) => MainSchemeId -> m ()
moveGlider target = do
  current <- getGliderSchemes
  for_ current $ \sid -> mainSchemeStep sid (SetMainSchemeGlider False)
  mainSchemeStep target (SetMainSchemeGlider True)

chooseGliderScheme :: (ReverseQueue m) => Bool -> IdentityId -> Text -> m ()
chooseGliderScheme most you continuation = do
  schemes <- selectList Matchers.MainScheme
  values <- for schemes $ \sid -> (sid,) <$> selectCount SchemeThreat (SchemeWithId sid)
  unless (null values) do
    let extreme = (if most then maximum else minimum) (map snd values)
    chooseOrRunOne you [targeting sid do
      moveGlider mid
      cardSpecific GameRef continuation (you, mid)
      | (sid@(SchemeMainSchemeId mid), n) <- values, n == extreme]

resolveGoblinSpecial :: (ReverseQueue m) => IdentityId -> MainSchemeId -> m ()
resolveGoblinSpecial you = \sid -> cardSpecific sid "goblinSpecial" you

instance RunMessage SmVenomGoblinMainScheme where
  runMessage msg e@(SmVenomGoblinMainScheme attrs) = runQueueT $ case msg of
    AdvanceMainScheme sid | sid == mainSchemeId attrs -> do
      let back = case toCardCode attrs of
            "27117" -> Just Environments.smLowerManhattanB
            "27118" -> Just Environments.smMidtownManhattanB
            "27119" -> Just Environments.smUpperManhattanB
            _ -> Nothing
      for_ back $ \def -> do
        cardSpecific GameRef "goblinFlip" (sid, def, mainSchemeAccelerationTokens attrs)
      pure e
    CardSpecific target "goblinSpecial" (fromJSON -> Success you) | isTarget attrs target -> do
      specialAbility you attrs $ cardSpecific attrs "resolveGoblinSpecial" (you :: IdentityId)
      pure e
    CardSpecific target "resolveGoblinSpecial" (fromJSON -> Success you) | isTarget attrs target -> do
      infested <- (> 0) <$> symbioteEnvironmentCount
      case toCardCode attrs of
        "27117" -> do
          schemes <- selectList AnyScheme
          for_ schemes $ \scheme -> placeThreat scheme 1
          when infested $ placeThreat (mainSchemeId attrs) 1
        "27118" -> dealIndirectDamage you attrs (if infested then 3 else 2)
        "27119" -> do
          discardFromHand you 1
          when infested $ discardFrom you FromDeck 4
        _ -> pure ()
      pure e
    _ -> SmVenomGoblinMainScheme <$> runMessage msg attrs
