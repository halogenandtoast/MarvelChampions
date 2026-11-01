module Marvel.Treachery.Treacheries.NextEvolutionMisterSinister where

import Marvel.Prelude
import Marvel.Ability ( HasAbilities )
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Keyword qualified as Keyword ()
import Marvel.Matchers
import Marvel.Modifier ( HasModifiersFor )
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.NextEvolution.Sinister

import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types


import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MisterSinisterTreachery = MisterSinisterTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40147 :: TreacheryCard MisterSinisterTreachery
ne40147 = treachery MisterSinisterTreachery Cards.ne40147

ne40148 :: TreacheryCard MisterSinisterTreachery
ne40148 = treachery MisterSinisterTreachery Cards.ne40148

ne40149 :: TreacheryCard MisterSinisterTreachery
ne40149 = treachery MisterSinisterTreachery Cards.ne40149

ne40150 :: TreacheryCard MisterSinisterTreachery
ne40150 = treachery MisterSinisterTreachery Cards.ne40150

instance RunMessage MisterSinisterTreachery where
  runMessage msg card@(MisterSinisterTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      villain <- sinister
      case toCardCode a of
        "40147" -> do
          whenM (sinisterHas Trait.Aerial) $ dealIndirectDamage you a 2
          whenM (sinisterHas Trait.Brute) $ exhaust you
          whenM (sinisterHas Trait.Psionic) $ placeThreatOnEach a 2 MainScheme
        "40148" -> do
          for_ villain (`giveStatus` ToughStatus)
          whenM (sinisterHas Trait.Brute) $ for_ villain (`heal` 4)
        "40149" -> do
          for_ villain (`villainSchemesAgainst` you)
          cardSpecific a "confuseIfPsionic" you
        "40150" -> do
          hero <- selectAny $ IdentityWithId you <> HeroIdentity
          if not hero then surge you else do
            for_ villain (`enemyAttacks` you)
            cardSpecific a "stunIfAerial" you
        _ -> pure ()
      pure card
    CardSpecific (isTarget a -> True) "confuseIfPsionic" (fromJSON -> Success you) -> card <$ whenM (sinisterHas Trait.Psionic) (confuseIdentity you)
    CardSpecific (isTarget a -> True) "stunIfAerial" (fromJSON -> Success you) -> card <$ whenM (sinisterHas Trait.Aerial) (stunIdentity you)
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "40148" -> sinister >>= traverse_ (`giveStatus` ToughStatus)
        "40149" -> confuseIdentity you
        "40150" -> stunIdentity you
        _ -> pure ()
      pure card
    _ -> MisterSinisterTreachery <$> runMessage msg a
