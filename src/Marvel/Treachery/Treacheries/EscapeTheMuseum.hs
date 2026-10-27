-- | Escape the Museum's treacheries (The Galaxy's Most Wanted).
module Marvel.Treachery.Treacheries.EscapeTheMuseum (
  iHaveYouNow,
  IHaveYouNow (..),
  impossibleGeometry,
  ImpossibleGeometry (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Keyword (Keyword (Permanent))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

iHaveYouNow :: TreacheryCard IHaveYouNow
iHaveYouNow = treachery IHaveYouNow Cards.iHaveYouNow

-- | When Revealed (Alter-Ego): exhaust your identity; Collector schemes. When Revealed
-- (Hero): you are stunned; Collector attacks you. [star] Boost: give Collector a tough
-- status card (one already tough cannot be given another). Collector is the villain.
newtype IHaveYouNow = IHaveYouNow (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage IHaveYouNow where
  runMessage msg t@(IHaveYouNow attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      collector <- selectOne ActiveVillain
      if hero
        then do
          stunIdentity you
          for_ collector (`enemyAttacks` you)
        else do
          exhaust you
          for_ collector villainSchemes
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      traverse_ enemyBecomesTough =<< selectOne ActiveVillain
      pure t
    _ -> IHaveYouNow <$> runMessage msg attrs

impossibleGeometry :: TreacheryCard ImpossibleGeometry
impossibleGeometry = treachery ImpossibleGeometry Cards.impossibleGeometry

-- | Incite 1, its card's. When Revealed: you are confused. If you are already confused,
-- choose and discard 1 card you control: an ally, upgrade or support, not a permanent
-- one, which cannot leave play.
newtype ImpossibleGeometry = ImpossibleGeometry (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage ImpossibleGeometry where
  runMessage msg t@(ImpossibleGeometry attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      confused <- identityMatches ConfusedIdentity you
      if confused
        then do
          let you' = IdentityWithId you
          allies <- selectList $ AllyControlledBy you'
          upgrades <- selectList $ UpgradeControlledBy you'
          supports <- selectList $ SupportControlledBy you'
          open <- flip filterM supports \s ->
            getsGame \g -> maybe True (notElem Permanent . cdKeywords) (abilityCardDef g (toTarget s))
          let controlled = map toTarget allies <> map toTarget upgrades <> map toTarget open
          unless (null controlled) $ chooseOne you [targeting c (removeFromPlay c) | c <- controlled]
        else confuseIdentity you
      pure t
    _ -> ImpossibleGeometry <$> runMessage msg attrs
