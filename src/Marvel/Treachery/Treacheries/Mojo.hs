module Marvel.Treachery.Treacheries.Mojo where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDamage))
import Marvel.Projection
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype MojoTreachery = MojoTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39032, mojo39033, mojo39034 :: TreacheryCard MojoTreachery
mojo39032 = treachery MojoTreachery Cards.mojo39032
mojo39033 = treachery MojoTreachery Cards.mojo39033
mojo39034 = treachery MojoTreachery Cards.mojo39034

instance RunMessage MojoTreachery where
  runMessage msg e@(MojoTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "39032" -> do
          characters <- selectList AnyCharacter
          counts <- traverse (\c -> (c,) <$> getCharacterThreat c) characters
          let most = maximum (0 : map snd counts)
          if most == 0
            then addThreat you 1
            else chooseOrRunOne you [targeting (characterRef c) (moveCharacterThreatToMainScheme c) | (c,n) <- counts, n == most]
        "39033" -> do
          let cost = DifferentResourcesCost 2
          payable <- canAffordCost you (toSource a) cost
          villain <- selectOne ActiveVillain
          n <- getCharacterThreat (IdentityCharacter you)
          chooseOne you $
            [labeled "$label.mojoSpendTwoDifferentResources" (payForTreachery you cost) | payable]
            <> [labeled "$label.villainSchemes" (villainSchemesAgainst v you) | Just v <- [villain]]
            <> [labeled "$label.mojoTakeThreatDamage" do
                  before <- project PlayerIdentityDamage you
                  damageIdentity you a (toDamage n FromAbility)
                  cardSpecific a "directorsDirectionsDamage" (you, before)]
        "39034" -> do
          controlled <- selectList (CharacterControlledBy $ IdentityWithId you)
          traverse_ (`heal` 1) controlled
          addThreat you 2
        _ -> pure ()
      pure e
    CardSpecific (isTarget a -> True) "directorsDirectionsDamage" (fromJSON -> Success (you, before :: Natural)) -> do
      present <- selectAny (IdentityWithId you)
      when present do
        after <- project PlayerIdentityDamage you
        when (after < before + 2) $ gainSurge a
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      if toCardCode a == "39032" then revealBoost a you else addThreat you 1
      pure e
    _ -> MojoTreachery <$> runMessage msg a
   where
    addThreat you n = selectList (CharacterControlledBy $ IdentityWithId you) >>= traverse_ (`placeCharacterThreat` n)
