module Marvel.Event.Events.RunningInterference (
  runningInterference,
  RunningInterference (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source (getVillainStage)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

runningInterference :: EventCard RunningInterference
runningInterference = event RunningInterference Cards.runningInterference

newtype RunningInterference = RunningInterference (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Play only if your identity has the Avenger trait (its criteria). Hero Action
-- (thwart): Remove 2 threat from the main scheme. Remove X additional threat from the
-- main scheme (to a maximum of 3), where X is equal to the villain's stage number: one
-- instance of 2 + X. No threat is removed while a crisis icon is in play.
instance RunMessage RunningInterference where
  runMessage msg e@(RunningInterference attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      stage <- maybe (pure 0) getVillainStage =<< selectOne ActiveVillain
      mains <- filter isMain <$> selectList ThwartableScheme
      -- none to remove it from (a crisis icon in play): nothing happens
      unless (null mains) $ chooseOrRunOne you
        [targeting (schemeRef scheme) (thwartScheme scheme attrs (2 + min 3 stage)) | scheme <- mains]
      pure e
    _ -> RunningInterference <$> runMessage msg attrs
   where
    isMain = \case
      SchemeMainSchemeId _ -> True
      _ -> False
