module Marvel.Treachery.Treacheries.MojoGenres where

import Data.List (nub)
import Marvel.Prelude
import Marvel.Card
import Marvel.Count (selectCount)
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype MojoGenreTreachery = MojoGenreTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39040, mojo39046, mojo39070 :: TreacheryCard MojoGenreTreachery
mojo39040 = treachery MojoGenreTreachery Cards.mojo39040
mojo39046 = treachery MojoGenreTreachery Cards.mojo39046
mojo39070 = treachery MojoGenreTreachery Cards.mojo39070

instance RunMessage MojoGenreTreachery where
  runMessage msg e@(MojoGenreTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      case toCardCode attrs of
        "39040" -> do
          schemes <- selectList AnySideScheme
          removable <- filterM (canMoveSchemeThreatFrom attrs . SchemeSideSchemeId) schemes
          chooseOne you $ [labeled "$label.mojoMoveSideSchemeThreat" (cardSpecific attrs "moveThreat" you) | not (null removable)]
            <> [labeled "$label.mojoRevealSideScheme" (discardUntil FromEncounterDeck (CardWithType SideSchemeType) attrs)]
        "39046" -> chooseOne you [labeled label do
          players <- getPlayers
          for_ players $ \p -> do
            drawCards p 2
            cardSpecific attrs "manaDrain" (p, resource)
          | (label, resource) <- [("$label.energy", Energy), ("$label.mental", Mental), ("$label.physical", Physical)]]
        "39070" -> do
          drawCards you 5
          cardSpecific attrs "chooseFive" you
        _ -> pure ()
      MojoGenreTreachery <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "chooseFive" (fromJSON -> Success you) -> do
      n <- min 5 . fromIntegral . length . unHand <$> project PlayerIdentityHand you
      discardFor you attrs FromHand n n
      pure e
    CardSpecific (isTarget attrs -> True) "moveThreat" (fromJSON -> Success you) -> do
      schemes <- selectList AnySideScheme
      removable <- filterM (canMoveSchemeThreatFrom attrs . SchemeSideSchemeId) schemes
      main <- selectList EncounterMainScheme
      unless (null removable || null main) $ chooseOne you [targeting sid do
        n <- selectCount SchemeThreat (SchemeWithId $ SchemeSideSchemeId sid)
        for_ (headMay main) $ \mainScheme -> moveSchemeThreat attrs (SchemeSideSchemeId sid) mainScheme n
        | sid <- removable]
      pure e
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- maybe getYou pure (treacheryResolver attrs)
      revealEncounterCard you card
      pure e
    CardSpecific (isTarget attrs -> True) "manaDrain" (fromJSON -> Success (you, resource)) -> do
      cards <- filter (cardMatch $ CardWithResource resource) . unHand <$> project PlayerIdentityHand you
      traverse_ discardCard cards
      pure e
    WithDiscarded (isTarget attrs -> True) FromHand cards | toCardCode attrs == "39070" -> do
      let resources = nub $ concatMap (map snd . filter ((== PrintedResource) . fst) . cdResources . getCardDef) cards
      you <- maybe getYou pure (treacheryResolver attrs)
      cardSpecific attrs "discardControlled" (you, fromIntegral (length resources) :: Natural)
      pure e
    CardSpecific (isTarget attrs -> True) "discardControlled" (fromJSON -> Success (you, n :: Natural)) -> do
      when (n > 0) $ do
        upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
        supports <- selectList (SupportControlledBy $ IdentityWithId you)
        chooseAndDiscardControlled you (map UpgradeRef upgrades <> map SupportRef supports)
        cardSpecific attrs "discardControlled" (you, n - 1)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target, toCardCode attrs == "39046" -> do
      you <- getBoostedPlayer enemy
      discardFromHand you 1
      pure e
    _ -> MojoGenreTreachery <$> runMessage msg attrs
