module Marvel.SideScheme.SideSchemes.RampagingJuggernaut where

import Marvel.Prelude
import Control.Monad (replicateM_)
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Game.Source
import Marvel.Projection
import Marvel.Identity.Types (Field (PlayerIdentityTough, PlayerIdentityAdditionalTough))

rampagingJuggernaut :: SideSchemeCard RampagingJuggernaut
rampagingJuggernaut = sideScheme RampagingJuggernaut Cards.rampagingJuggernaut (PerPlayer 2)

newtype RampagingJuggernaut = RampagingJuggernaut (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage RampagingJuggernaut where
  runMessage msg e@(RampagingJuggernaut attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      counts <- for players \you -> do
        tough <- project PlayerIdentityTough you
        extra <- project PlayerIdentityAdditionalTough you
        pure (you, (if tough then 1 else 0) + extra)
      allies <- selectList AnyAlly
      toughAllies <- filterM (characterMatches (CharacterWithStatus ToughStatus) . AllyCharacter) allies
      for_ counts \(you, n) -> replicateM_ (fromIntegral n) (removeStatus you ToughStatus)
      for_ toughAllies (\aid -> removeStatus aid ToughStatus)
      placeThreat (sideSchemeId attrs) (2 * (sum (map snd counts) + fromIntegral (length toughAllies)))
      pure e
    _ -> RampagingJuggernaut <$> runMessage msg attrs
