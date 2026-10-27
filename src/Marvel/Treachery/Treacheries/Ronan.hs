-- | Ronan the Accuser's treacheries (The Galaxy's Most Wanted, scenario 5).
module Marvel.Treachery.Treacheries.Ronan (
  singleMindedFury,
  SingleMindedFury (..),
  kreePhysiology,
  KreePhysiology (..),
  youStandAccused,
  YouStandAccused (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Treacheries.BadoonHeadhunter (anotherBoostCard)
import Marvel.Treachery.Types

singleMindedFury :: TreacheryCard SingleMindedFury
singleMindedFury = treachery (SingleMindedFury . (`With` Meta False Nothing)) Cards.singleMindedFury

-- | Whether Ronan attacked as this card told him to, while it resolves.
data Meta = Meta {attackMade :: Bool, attacker :: Maybe VillainId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SingleMindedFury = SingleMindedFury (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery SingleMindedFury where
  toTreacheryAttrs (SingleMindedFury (attrs `With` _)) = attrs

-- | When Revealed: Ronan the Accuser attacks the player who controls the Power Stone
-- (even in alter-ego form). If no attack was made this way (no player controls it, or
-- his stun is removed instead), this card gains surge. [star] Boost: attach the Power
-- Stone to Ronan the Accuser.
instance RunMessage SingleMindedFury where
  runMessage msg t@(SingleMindedFury (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        holder <- getPowerStoneHolder
        villain <- selectOne ActiveVillain
        attacker' <- case (villain, holder) of
          (Just v, Just (IdentityCharacter p)) -> Just v <$ enemyAttacks v p
          _ -> pure Nothing
        -- only the attack of Ronan's this reveal makes counts
        pure $ SingleMindedFury (attrs `With` Meta False attacker')
      ResolvedTreachery _ | not (attackMade meta) ->
        SingleMindedFury . (`With` meta) <$> runMessage msg (attrs & surgeL .~ True)
      _ -> SingleMindedFury . (`With` meta) <$> runMessage msg attrs
    VillainMessage vid VillainAttacked | Just vid == attacker meta -> pure $ SingleMindedFury (attrs `With` meta {attackMade = True})
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      selectOne ActiveVillain >>= traverse_ attachPowerStoneTo
      pure t
    _ -> SingleMindedFury . (`With` meta) <$> runMessage msg attrs

kreePhysiology :: TreacheryCard KreePhysiology
kreePhysiology = treachery KreePhysiology Cards.kreePhysiology

newtype KreePhysiology = KreePhysiology (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Surge (its card's). When Revealed: give Ronan the Accuser a tough status card. If he
-- already has a tough status card, take 1 damage.
instance RunMessage KreePhysiology where
  runMessage msg t@(KreePhysiology attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      selectOne ActiveVillain >>= traverse_ \v -> do
        tough <- villainMatches VillainWithToughStatus v
        if tough
          then damageIdentity you attrs (toDamage 1 FromTreachery)
          else enemyBecomesTough v
      pure t
    _ -> KreePhysiology <$> runMessage msg attrs

youStandAccused :: TreacheryCard YouStandAccused
youStandAccused = treachery YouStandAccused Cards.youStandAccused

newtype YouStandAccused = YouStandAccused (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed (Alter-Ego): Ronan the Accuser schemes with +1 SCH. (Hero): he attacks
-- you with +1 ATK. [star] Boost: give the villain 1 additional boost card for this
-- activation.
instance RunMessage YouStandAccused where
  runMessage msg t@(YouStandAccused attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      selectOne ActiveVillain >>= traverse_ \v -> do
        activationBonus v 1
        if hero then enemyAttacks v you else villainSchemes v
      pure t
    _ | Just boost <- anotherBoostCard attrs msg -> t <$ boost
    _ -> YouStandAccused <$> runMessage msg attrs
