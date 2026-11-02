-- | The Space Pirates modular set's minions (The Galaxy's Most Wanted): each has
-- quickstrike (its card's), "[star] Forced Response: after this minion attacks and
-- damages you, …" and "[star] Boost: give the villain 1 additional boost card for this
-- activation".
module Marvel.Minion.Minions.SpacePirates (
  pirateCommander,
  PirateCommander (..),
  pirateLackey,
  PirateLackey (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Deck
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityDeck, PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Projection
import Marvel.Window
import System.Random.Shuffle (shuffleM)

-- | After the minion attacks and damages you.
afterDamagingYou :: Attrs Minion -> Ability
afterDamagingYou a =
  windowAbility a 1 (EnemyAttackedAndDamaged (EnemyWithId $ EnemyMinionId $ minionId a) (IdentityCharacters AnyIdentity)) ForcedResponse NoCost
    $ RunAbility (toTarget a) 1

-- | The boost for the villain's activation.
anotherBoostCard :: (ReverseQueue m) => Attrs Minion -> Message -> Maybe (m ())
anotherBoostCard attrs = \case
  Boost (RevealedAsBoost target enemy) | isTarget attrs target -> Just case enemy of
    EnemyVillainId villain -> dealBoost villain
    -- turned up for a villainous minion's, it still gives "the villain" its card (RR
    -- Villainous), facedown on him for his next activation
    EnemyMinionId _ -> selectOne ActiveVillain >>= traverse_ dealBoost
    EnemyAttachmentId _ -> pure ()
  _ -> Nothing

pirateCommander :: MinionCard PirateCommander
pirateCommander = minion PirateCommander Cards.pirateCommander (Sch 2) (Atk 2) (HP 6)

-- | Forced Response: remove 1 card at random in your hand from the game.
newtype PirateCommander = PirateCommander (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PirateCommander where
  getAbilities (PirateCommander a) = [afterDamagingYou a]

instance RunMessage PirateCommander where
  runMessage msg e@(PirateCommander attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttacksAndDamages _ (IdentityCharacter you) _] _ -> do
      hand <- projectMap PlayerIdentityHand unHand you
      picked <- take 1 <$> shuffleM hand
      for_ picked (removeFromGame . CardIdRef . pcCardId)
      pure e
    _ | Just boost <- anotherBoostCard attrs msg -> e <$ boost
    _ -> PirateCommander <$> runMessage msg attrs

pirateLackey :: MinionCard PirateLackey
pirateLackey = minion PirateLackey Cards.pirateLackey (Sch 1) (Atk 2) (HP 3)

-- | Forced Response: remove the top card of your deck from the game.
newtype PirateLackey = PirateLackey (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PirateLackey where
  getAbilities (PirateLackey a) = [afterDamagingYou a]

instance RunMessage PirateLackey where
  runMessage msg e@(PirateLackey attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttacksAndDamages _ (IdentityCharacter you) _] _ -> do
      top <- projectMap PlayerIdentityDeck (take 1 . unDeck) you
      for_ top (removeFromGame . CardIdRef . pcCardId)
      pure e
    _ | Just boost <- anotherBoostCard attrs msg -> e <$ boost
    _ -> PirateLackey <$> runMessage msg attrs
