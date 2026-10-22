module Marvel.Treachery.Treacheries.EnergyBlast (
  energyBlast,
  EnergyBlast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

energyBlast :: TreacheryCard EnergyBlast
energyBlast = treachery EnergyBlast Cards.energyBlast

newtype EnergyBlast = EnergyBlast (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed (Alter-Ego): discard an ally or support you control; if you cannot,
-- this card gains surge. When Revealed (Hero): Kang attacks you.
instance RunMessage EnergyBlast where
  runMessage msg t@(EnergyBlast attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      isHero <- identityMatches HeroIdentity you
      if isHero
        then t <$ (traverse_ (`enemyAttacks` you) =<< selectOne ActiveVillain)
        else do
          allies <- selectList (AllyControlledBy $ IdentityWithId you)
          supports <- selectList (SupportControlledBy $ IdentityWithId you)
          let cards = map AllyRef allies <> map SupportRef supports
          if null cards
            then pure . EnergyBlast $ attrs & surgeL .~ True
            else t <$ chooseOne you [targeting c (discardControlled c) | c <- cards]
    _ -> EnergyBlast <$> runMessage msg attrs
