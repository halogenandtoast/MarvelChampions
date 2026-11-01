module Marvel.Treachery.Treacheries.NextEvolutionNeStryfe where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.NextEvolution.Stryfe
import Marvel.Projection
import Marvel.Query
import Marvel.Trait (Trait (Psionic))
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NeStryfeTreachery = NeStryfeTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40177 :: TreacheryCard NeStryfeTreachery
ne40177 = treachery NeStryfeTreachery Cards.ne40177

ne40178 :: TreacheryCard NeStryfeTreachery
ne40178 = treachery NeStryfeTreachery Cards.ne40178

ne40179 :: TreacheryCard NeStryfeTreachery
ne40179 = treachery NeStryfeTreachery Cards.ne40179

instance RunMessage NeStryfeTreachery where
  runMessage msg e@(NeStryfeTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      case toCardCode a of
        "40177" -> do
          n <- handTypeCount you
          discardTopOfEncounterDeckThen n $ \cards ->
            for_ (filter (cardMatch $ CardWithTrait Psionic) cards) (dealFoundEncounterCard you)
        "40178" -> chooseOne you
          [labeled label do
             cards <- unHand <$> project PlayerIdentityHand you
             for_ (filter ((/= typ) . cdCardType . getCardDef) cards) discardCard
             drawToHandSize you
             cardSpecific a "neOverrideThreat" (you, typ)
          | (typ, label) <- [(AllyType,"$label.ally"),(EventType,"$label.event"),(ResourceType,"$label.resource"),(SupportType,"$label.support"),(UpgradeType,"$label.upgrade"),(PlayerSideSchemeType,"$label.playerSideScheme")]]
        "40179" -> do
          returnControlledCard you
          villain <- selectJust ActiveVillain
          villainActivatesAgainst villain you
        _ -> pure ()
      pure e
    CardSpecific target "neOverrideThreat" (fromJSON -> Success (you, typ)) | target == toRef a -> do
      cards <- unHand <$> project PlayerIdentityHand you
      let n = fromIntegral $ length $ filter ((== typ) . cdCardType . getCardDef) cards
      placeThreatOnEach a n EncounterMainScheme
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      case toCardCode a of
        "40178" -> discardFromHand you 1 >> drawCards you 1
        "40179" -> do
          n <- handTypeCount you
          when (n >= 3) $ placeThreatOnEach a 3 EncounterMainScheme
        _ -> pure ()
      pure e
    _ -> NeStryfeTreachery <$> runMessage msg a
