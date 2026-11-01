module Marvel.Treachery.Treacheries.TheEyeOfSauron where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Treachery.Types

theEyeOfSauron :: TreacheryCard TheEyeOfSauron
theEyeOfSauron = treachery TheEyeOfSauron Cards.theEyeOfSauron

newtype TheEyeOfSauron = TheEyeOfSauron (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TheEyeOfSauron where
  runMessage msg a@(TheEyeOfSauron attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      present <- selectAny (MinionIs Cards.sauron)
      discardFromDeck you (if present then 3 else 2) attrs
      pure a
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let pcs = onlyPlayerCards cards
          icons = concatMap (printedResources . getCardDef) pcs
      for_ (headMay pcs >>= pcOwner) $ \you -> unless (null icons) $ chooseOneAtATime you
        [labeled (case icon of Energy -> "$label.energy"; Mental -> "$label.mental"; Physical -> "$label.physical"; Wild -> "$label.wild") $ cardSpecific attrs "icon" (you, icon) | icon <- icons]
      pure a
    CardSpecific (isTarget attrs -> True) "icon" (fromJSON -> Success (you, icon)) -> do
      case icon of
        Energy -> placeThreatOnEach attrs 1 MainScheme
        Mental -> discardFrom you FromHand 1
        Physical -> damageIdentity you attrs (toDamage 1 FromAbility)
        Wild -> do
          identities <- selectList (IdentityWithId you <> UnexhaustedIdentity)
          allies <- selectList (AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly)
          let characters = map IdentityCharacter identities <> map AllyCharacter allies
          chooseOne you [targeting (characterRef character) $ case character of IdentityCharacter p -> exhaust p; AllyCharacter aid -> exhaust aid; _ -> pure () | character <- characters]
      pure a
    _ -> TheEyeOfSauron <$> runMessage msg attrs
