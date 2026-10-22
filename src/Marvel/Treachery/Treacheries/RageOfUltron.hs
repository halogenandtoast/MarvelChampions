module Marvel.Treachery.Treacheries.RageOfUltron (
  rageOfUltron,
  RageOfUltron (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

rageOfUltron :: TreacheryCard RageOfUltron
rageOfUltron = treachery RageOfUltron Cards.rageOfUltron

newtype RageOfUltron = RageOfUltron (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | While this resolves, its player discards the top card of their deck for each threat
-- Ultron's scheme places (alter-ego) or each damage his attack deals (hero).
instance RunMessage RageOfUltron where
  runMessage msg t@(RageOfUltron attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity identityId
      villainId <- selectJust ActiveVillain
      if isHero then enemyAttacks villainId identityId else villainSchemes villainId
      pure . RageOfUltron $ attrs & resolverL ?~ identityId
    MainSchemeMessage _ (MainSchemePlaceThreat n) | Just identityId <- treacheryResolver attrs -> do
      isHero <- identityMatches HeroIdentity identityId
      unless isHero $ discardTop identityId n
      pure t
    IdentityMessage _ (IdentityDamaged source damage) -> attackDamage source damage >> pure t
    AllyMessage _ (AllyDamaged source damage) -> attackDamage source damage >> pure t
    _ -> RageOfUltron <$> runMessage msg attrs
   where
    discardTop identityId n = when (n > 0) $ discardFrom identityId FromDeck n
    attackDamage source damage = case (source, treacheryResolver attrs) of
      (VillainRef _, Just identityId) | damageSource damage == FromAttack -> do
        isHero <- identityMatches HeroIdentity identityId
        when isHero $ discardTop identityId (damageAmount damage)
      _ -> pure ()
