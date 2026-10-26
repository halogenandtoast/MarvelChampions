module Marvel.Treachery.Treacheries.FullAuto (
  fullAuto,
  FullAuto (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref (IsRef)
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

fullAuto :: TreacheryCard FullAuto
fullAuto = treachery FullAuto Cards.fullAuto

newtype FullAuto = FullAuto (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Alter-ego: surge. Hero: discard X cards from the encounter deck, X being Crossbones'
-- ATK, and take 1 indirect damage for each boost icon discarded.
instance RunMessage FullAuto where
  runMessage msg t@(FullAuto attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      if hero
        then do
          x <- maybe (pure 0) getVillainAttackValue =<< selectOne ActiveVillain
          discardTopOfEncounterDeckThen x (countBoostIcons attrs you)
          pure t
        else pure . FullAuto $ attrs & surgeL .~ True
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) -> do
      when (n > 0) $ dealIndirectDamage you attrs n
      pure t
    _ -> FullAuto <$> runMessage msg attrs
