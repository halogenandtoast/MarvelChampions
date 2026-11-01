module Marvel.Treachery.Treacheries.AgeOfApocalypseEnSabahNur where

import Marvel.Prelude
import Marvel.Game.Source ()
import Marvel.Scenario.AgeOfApocalypseEnSabahNur
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

newtype AoaEnSabahNurTreachery = AoaEnSabahNurTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

biomorphicBlast :: TreacheryCard AoaEnSabahNurTreachery
biomorphicBlast = treachery AoaEnSabahNurTreachery Cards.biomorphicBlast

technologicalInterface :: TreacheryCard AoaEnSabahNurTreachery
technologicalInterface = treachery AoaEnSabahNurTreachery Cards.technologicalInterface

giantSizedDespot :: TreacheryCard AoaEnSabahNurTreachery
giantSizedDespot = treachery AoaEnSabahNurTreachery Cards.giantSizedDespot


instance RunMessage AoaEnSabahNurTreachery where
  runMessage msg a@(AoaEnSabahNurTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      same <- apocalypseInForm form
      if same then enSabahNur >>= traverse_ (`villainActivatesAgainst` you)
        else changeApocalypseForm form >> placeApocalypsePower
      pure a
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      enemyActivationAfterCurrent enemy $ changeApocalypseForm form
      pure a
    _ -> AoaEnSabahNurTreachery <$> runMessage msg attrs
   where
    form = case toCardCode attrs of
      "45150" -> "Biomorph"
      "45151" -> "Cyberpath"
      _ -> "Giant"
