{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw53019 where
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
import Marvel.Event.Types

import Data.List (nub)
import Marvel.Trait (getTraits)
fw53019 :: EventCard Fw53019
fw53019 = event Fw53019 Cards.fw53019

newtype Fw53019 = Fw53019 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw53019 where
  runMessage msg a@(Fw53019 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment windows) | ident == eventId attrs -> do
      characters <- selectList $ CharacterControlledBy AnyIdentity
      traits <- concatMapM (fmap toList . getCharacterTraits) characters
      cardSpecific attrs "diversity" (you, fromIntegral (length $ nub traits) :: Natural)
      pure a
    CardSpecific (isTarget attrs -> True) "diversity" (fromJSON -> Success (you, remaining :: Natural)) -> do
      when (remaining > 0) $ do
        enemies <- selectList AnyEnemy
        schemes <- selectList ThwartableScheme
        chooseOne you $
          [targeting (enemyRef enemy) (dealDamage enemy attrs (toDamage 1 FromAbility) >> cardSpecific attrs "diversity" (you, remaining - 1)) | enemy <- enemies]
            <> [targeting (schemeRef scheme) (thwartSchemeAs you scheme attrs 1 >> cardSpecific attrs "diversity" (you, remaining - 1)) | scheme <- schemes]
      pure a
    _ -> Fw53019 <$> runMessage msg attrs
