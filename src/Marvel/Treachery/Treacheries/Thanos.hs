-- | The Thanos set's treacheries (The Mad Titan's Shadow, scenario 3): "Thanos" is the
-- villain.
module Marvel.Treachery.Treacheries.Thanos (
  avatarOfDeath,
  AvatarOfDeath (..),
  deviantSyndrome,
  DeviantSyndrome (..),
  iAmInevitable,
  IAmInevitable (..),
  theMadTitan,
  TheMadTitan (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

avatarOfDeath :: TreacheryCard AvatarOfDeath
avatarOfDeath = treachery AvatarOfDeath Cards.avatarOfDeath

newtype AvatarOfDeath = AvatarOfDeath (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): Thanos schemes. When Revealed (Hero): Thanos attacks you;
-- that attack gains overkill and piercing.
instance RunMessage AvatarOfDeath where
  runMessage msg t@(AvatarOfDeath attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      selectOne ActiveVillain >>= traverse_ \v -> do
        hero <- identityMatches HeroIdentity you
        if hero then enemyAttacksGaining v you [Overkill, Piercing] else villainSchemesAgainst v you
      pure t
    _ -> AvatarOfDeath <$> runMessage msg attrs

deviantSyndrome :: TreacheryCard DeviantSyndrome
deviantSyndrome = treachery DeviantSyndrome Cards.deviantSyndrome

newtype DeviantSyndrome = DeviantSyndrome (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Incite 1 (its keyword). When Revealed: give Thanos a tough status card; if you cannot
-- (he has one), place 2 threat on the main scheme. [star] Boost: give Thanos a tough
-- status card.
instance RunMessage DeviantSyndrome where
  runMessage msg t@(DeviantSyndrome attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      selectOne ActiveVillain >>= traverse_ \v -> do
        tough <- selectAny (VillainWithId v <> VillainWithToughStatus)
        if tough then placeThreatOnEach attrs 2 EncounterMainScheme else enemyBecomesTough v
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      traverse_ enemyBecomesTough =<< selectOne ActiveVillain
      pure t
    _ -> DeviantSyndrome <$> runMessage msg attrs

iAmInevitable :: TreacheryCard IAmInevitable
iAmInevitable = treachery IAmInevitable Cards.iAmInevitable

newtype IAmInevitable = IAmInevitable (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: give Thanos 1 facedown boost card. [star] Boost: discard the top card
-- of the Infinity Stone deck; its boost icons count for this activation as a boost
-- card's would.
instance RunMessage IAmInevitable where
  runMessage msg t@(IAmInevitable attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      traverse_ dealBoost =<< selectOne ActiveVillain
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      discardTopInfinityStoneThen 1 (countActivationBoostIcons (enemyRef enemy))
      pure t
    _ -> IAmInevitable <$> runMessage msg attrs

theMadTitan :: TreacheryCard TheMadTitan
theMadTitan = treachery (TheMadTitan . (`With` Meta Nothing)) Cards.theMadTitan

-- | The enemy whose activation it is a boost card for.
newtype Meta = Meta {boosting :: Maybe EnemyId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheMadTitan = TheMadTitan (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery TheMadTitan where
  toTreacheryAttrs (TheMadTitan (attrs `With` _)) = attrs

-- | When Revealed: put the top card of the Infinity Stone deck into play. [star] Boost:
-- if damage from this attack defeats an ally, put the top card of the Infinity Stone
-- deck into play (for the player attacked).
instance RunMessage TheMadTitan where
  runMessage msg t@(TheMadTitan (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      putTopInfinityStoneIntoPlay you
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> pure $ TheMadTitan (attrs `With` Meta (Just enemy))
    Boost (CheckWindows ws)
      | Just enemy <- boosting meta
      , notNull [() | W.Window _ (W.DefeatedAlly _ (Just e)) <- ws, e == enemy] -> do
          putTopInfinityStoneIntoPlay =<< getBoostedPlayer enemy
          pure t
    _ -> TheMadTitan . (`With` meta) <$> runMessage msg attrs
