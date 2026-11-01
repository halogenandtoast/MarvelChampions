module Marvel.AlterEgo.AlterEgos.BobbyDrake where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers
import Marvel.AlterEgo.Runner
import Marvel.Stats

bobbyDrake :: AlterEgoCard BobbyDrake
bobbyDrake = alterEgo BobbyDrake Cards.bobbyDrake (HP $ Static 11) (HandSize 6) (Rec 4) [Cards.hotHeaded]

newtype BobbyDrake = BobbyDrake (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BobbyDrake where
  getAbilities a@(BobbyDrake attrs) =
    [ ability a 1 Setup IsSelf NoCost (runAbility a 1)
    , label "$label.coolOff" $ limitedWindowAbility a 2
        (W.IdentityChangedToForm W.After $ IdentityWithId $ alterEgoIdentityId attrs)
        Response (IsSelf <> UpgradeExists (UpgradeIs Cards.frostbite) <> ExtendedCardExists (InDiscardOf (IdentityWithId $ alterEgoIdentityId attrs) $ BasicCardMatches $ CardWithTrait Trait.Ice)) NoCost (runAbility a 2)
    ]
instance RunMessage BobbyDrake where
  runMessage msg a@(BobbyDrake attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- for [1..6 :: Int] $ \_ -> do
        cid <- getRandom
        pure $ PlayerCard $ MkPlayerCard cid Cards.frostbite (Just you) (Just you)
      setAside cards
      pure a
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      n <- length <$> selectList (UpgradeIs Cards.frostbite)
      cardSpecific attrs "coolOff" (you, n)
      pure a
    CardSpecific (isTarget attrs -> True) "coolOff" (fromJSON -> Success (you, n :: Int)) -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithTrait Trait.Ice)
      when (n > 0 && notNull cards) $ chooseOne you [targeting c $ do
        shuffleIntoDeck you [c]
        cardSpecific attrs "coolOff" (you, n-1) | c <- cards]
      pure a
    CardSpecific (IdentityRef you) "icemanDelayedFrostbite" (fromJSON -> Success enemy) | you == alterEgoIdentityId attrs -> a <$ attachFrostbite you enemy
    _ -> BobbyDrake <$> runMessage msg attrs
