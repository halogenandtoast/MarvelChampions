module Marvel.CampaignPlayers.NemesisTreacheries where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Card
import Marvel.CampaignPlayers.EncounterCards qualified as Cards
import Marvel.Count
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

cn56058, cn60036, cn60064 :: TreacheryCard NemesisTreachery
cn56058 = treachery NemesisTreachery Cards.enc56058
cn60036 = treachery NemesisTreachery Cards.enc60036
cn60064 = treachery NemesisTreachery Cards.enc60064

newtype NemesisTreachery = NemesisTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage NemesisTreachery where
  runMessage msg e@(NemesisTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> case toCardCode a of
      "56058" -> do
        discardFromDeck you 3 a
        pure e
      "60036" -> do
        allies <- selectRefs $ AllyControlledBy (IdentityWithId you)
        supports <- selectRefs $ SupportControlledBy (IdentityWithId you) <> SupportWithTrait T.Persona
        chooseOne you $ [targeting target (removeFromGame target) | target <- allies <> supports]
          <> [labeled "$label.cnBullseyeAttacks" $ do
            bullseyes <- selectList $ MinionIs Cards.enc60033
            if null bullseyes then searchEverywhereAndRevealEncounterCard you (CardIs Cards.enc60033)
            else traverse_ (`enemyAttacks` you) bullseyes]
        pure e
      "60064" -> do
        hero <- identityMatches HeroIdentity you
        if hero then do
          heroes <- selectList HeroIdentity
          chooseOne you [targeting who $ cardSpecific a "damageChosenHero" who | who <- heroes]
          pure e
        else do
          kingpins <- selectList $ MinionIs Cards.enc60061
          if null kingpins then pure $ NemesisTreachery $ a & surgeL .~ True
          else do
            traverse_ (`minionSchemesAgainst` you) kingpins
            pure e
      _ -> pure e
    WithDiscarded (isTarget a -> True) FromDeck (onlyPlayerCards -> cards) | toCardCode a == "56058" -> do
      you <- getYou
      let n = fromIntegral $ length $ nub [r | card <- cards, (PrintedResource, r) <- cdResources $ getCardDef card]
      hero <- identityMatches HeroIdentity you
      if hero then damageIdentity you a (toDamage n FromAbility)
      else placeThreatOnEach a n MainScheme
      pure e
    CardSpecific (isTarget a -> True) "damageChosenHero" (fromJSON -> Success (you :: IdentityId)) -> do
      n <- selectCount HeroAttackDamage (IdentityWithId you)
      damageIdentity you a (toDamage n FromAbility)
      pure e
    _ -> NemesisTreachery <$> runMessage msg a
