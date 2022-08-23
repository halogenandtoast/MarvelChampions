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
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Types
import qualified Marvel.Treachery.Cards as Cards

soundManipulation :: TreacheryCard SoundManipulation
soundManipulation = treachery SoundManipulation Cards.soundManipulation

newtype SoundManipulation = SoundManipulation TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage SoundManipulation where
  runMessage msg t@(SoundManipulation attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery ident -> do
          isHero <- identityMatches HeroIdentity ident
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
                    ident
                    (IdentityDamaged (toSource attrs) (toDamage 2 FromAbility))
                , VillainMessage villainId (VillainHealed 2)
                ]
              pure t
        _ -> SoundManipulation <$> runMessage msg attrs
    _ -> SoundManipulation <$> runMessage msg attrs
