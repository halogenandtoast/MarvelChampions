module Marvel.Treachery.Treacheries.AgeOfApocalypseCelestialTech where

import Marvel.Prelude
import Marvel.Attachment.Types ()
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait qualified as Trait
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaCelestialTechTreachery = AoaCelestialTechTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

celestialTech :: TreacheryCard AoaCelestialTechTreachery
celestialTech = treachery AoaCelestialTechTreachery Cards.celestialTech


instance RunMessage AoaCelestialTechTreachery where
  runMessage msg a@(AoaCelestialTechTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      attachments <- selectList $ AttachmentWithTrait Trait.Celestial
      if null attachments then searchAndRevealEncounterCard you (CardWithType AttachmentType <> CardWithTrait Trait.Celestial)
        else for_ attachments $ \aid -> cardSpecific aid "resolveCelestial" you
      pure a
    _ -> AoaCelestialTechTreachery <$> runMessage msg attrs
