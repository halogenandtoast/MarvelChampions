module Marvel.Treachery.Treacheries.SoundManipulation (
  soundManipulation,
  SoundManipulation (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

soundManipulation :: TreacheryCard SoundManipulation
soundManipulation = treachery SoundManipulation Cards.soundManipulation

newtype SoundManipulation = SoundManipulation (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage SoundManipulation where
  runMessage msg t@(SoundManipulation attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        isHero <- identityMatches HeroIdentity identityId
        villainId <- selectJust ActiveVillain
        damaged <- villainMatches VillainWithAnyDamage villainId
        case (isHero, damaged) of
          (False, False) -> pure . SoundManipulation $ attrs & surgeL .~ True
          (False, True) -> do
            push $ VillainMessage villainId (VillainHealed 4)
            pure t
          (True, _) -> do
            pushAll
              [ IdentityMessage
                  identityId
                  (IdentityDamaged (toSource attrs) (toDamage 2 FromAbility))
              , VillainMessage villainId (VillainHealed 2)
              ]
            pure t
      _ -> SoundManipulation <$> runMessage msg attrs
    _ -> SoundManipulation <$> runMessage msg attrs
