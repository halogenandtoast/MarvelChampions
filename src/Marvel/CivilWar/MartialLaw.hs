module Marvel.CivilWar.MartialLaw where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

newtype ShieldSoldier = ShieldSoldier (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

shieldSoldier :: MinionCard ShieldSoldier
shieldSoldier = minion ShieldSoldier C.cw56109 (Sch 1) (Atk 1) (HP 3)

instance RunMessage ShieldSoldier where
  runMessage msg e@(ShieldSoldier a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      discard <- asYou you getEncounterDiscard
      for_ (headMay $ filter (cardMatch $ CardWithType MinionType <> CardWithTrait T.Shield) discard) $
        putEncounterCardIntoPlay you
      pure e
    Boost (RevealedAsBoost target _) | isTarget a target -> do
      you <- getActivePlayerId
      putBoostIntoPlay target you
      pure e
    _ -> ShieldSoldier <$> runMessage msg a

newtype RapidResponse = RapidResponse (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

rapidResponse :: TreacheryCard RapidResponse
rapidResponse = treachery RapidResponse C.cw56110

instance RunMessage RapidResponse where
  runMessage msg e@(RapidResponse a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      discardUntil FromEncounterDeck (CardWithType MinionType) a
      pure $ RapidResponse a {treacheryResolver = Just you}
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) -> do
      discardCard card
      for_ (treacheryResolver a) $ \you -> do
        hero <- identityMatches HeroIdentity you
        if hero
          then damageIdentity you a (toDamage (encounterCardPrintedAttack you card) FromAbility)
          else do
            schemes <- asYou you $ selectList EncounterMainScheme
            for_ schemes $ \scheme -> placeThreatOn a scheme (encounterCardPrintedScheme you card)
      pure e
    _ -> RapidResponse <$> runMessage msg a

newtype ShieldPatrol = ShieldPatrol (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

shieldPatrol :: SideSchemeCard ShieldPatrol
shieldPatrol = sideScheme ShieldPatrol C.cw56111 (Static 3)

instance HasModifiersFor ShieldPatrol where
  getModifiersFor _ (MinionRef mid) (ShieldPatrol a) = do
    sameBoard <- (==) <$> getAreaOfTarget a <*> getAreaOfTarget mid
    pure [KeywordModifier K.Guard | sameBoard]
  getModifiersFor _ _ _ = pure []

instance RunMessage ShieldPatrol where
  runMessage msg (ShieldPatrol a) = ShieldPatrol <$> runMessage msg a
