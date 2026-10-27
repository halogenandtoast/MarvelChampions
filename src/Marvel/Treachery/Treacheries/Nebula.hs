-- | The Nebula encounter set's treacheries (The Galaxy's Most Wanted).
module Marvel.Treachery.Treacheries.Nebula (
  barrelRoll,
  BarrelRoll (..),
  combatReady,
  CombatReady (..),
  ruthless,
  Ruthless (..),
) where

import Marvel.Prelude

import Marvel.Attachment.Attachments.Techniques (resolveSpecial)
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Environments.NebulasShip (placeEvasionCounters)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

barrelRoll :: TreacheryCard BarrelRoll
barrelRoll = treachery BarrelRoll Cards.barrelRoll

-- | Incite 1. Surge (its keywords). When Revealed: place 1 evasion counter on Nebula's
-- Ship. [star] Boost: place 1 evasion counter on Nebula's Ship.
newtype BarrelRoll = BarrelRoll (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BarrelRoll where
  runMessage msg t@(BarrelRoll attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> t <$ placeEvasionCounters 1
    Boost (RevealedAsBoost target _) | isTarget attrs target -> t <$ placeEvasionCounters 1
    _ -> BarrelRoll <$> runMessage msg attrs

combatReady :: TreacheryCard CombatReady
combatReady = treachery CombatReady Cards.combatReady

-- | When Revealed: discard cards from the top of the encounter deck until a Technique
-- attachment is discarded; reveal it, then resolve its Special ability.
newtype CombatReady = CombatReady (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CombatReady where
  runMessage msg t@(CombatReady attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      discardUntil FromEncounterDeck (CardWithType AttachmentType <> CardWithTrait Technique) attrs
      pure . CombatReady $ attrs & resolverL ?~ you
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      for_ (treacheryResolver attrs) \you -> do
        revealEncounterCard you card
        resolveSpecial (AttachmentId $ unCardId $ ecCardId card) you
      pure t
    _ -> CombatReady <$> runMessage msg attrs

ruthless :: TreacheryCard Ruthless
ruthless = treachery (Ruthless . (`With` Meta Nothing False False)) Cards.ruthless

-- | The villain activating, whether it is placing its scheme's threat now, and whether
-- the activation has placed threat or dealt damage.
data Meta = Meta {activating :: Maybe VillainId, scheming :: Bool, succeeded :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Ruthless = Ruthless (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery Ruthless where
  toTreacheryAttrs (Ruthless (attrs `With` _)) = attrs

-- | When Revealed (Alter-Ego): Nebula schemes; if threat is placed by this activation,
-- place 1 evasion counter on Nebula's Ship. (Hero): Nebula attacks you; if damage is
-- dealt by this activation, place 1 evasion counter on Nebula's Ship.
instance RunMessage Ruthless where
  runMessage msg t@(Ruthless (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      hero <- identityMatches HeroIdentity you
      selectOne ActiveVillain >>= \case
        Nothing -> pure t
        Just v -> do
          if hero then enemyAttacks v you else villainSchemes v
          cardSpecific attrs "ruthlessEvasion" ()
          pure $ Ruthless (attrs `With` meta {activating = Just v})
    VillainMessage v VillainSchemed | activating meta == Just v -> pure $ Ruthless (attrs `With` meta {scheming = True})
    SideSchemeMessage _ (SideSchemePlaceThreat n) | scheming meta -> placed n
    MainSchemeMessage _ (MainSchemePlaceThreat n) | scheming meta -> placed n
    (attackDamageWindow -> Just (EnemyVillainId v, _, n)) | activating meta == Just v, n > 0 ->
      pure $ Ruthless (attrs `With` meta {succeeded = True})
    CardSpecific (isTarget attrs -> True) "ruthlessEvasion" _ -> do
      when (succeeded meta) $ placeEvasionCounters 1
      pure $ Ruthless (attrs `With` Meta Nothing False False)
    _ -> Ruthless . (`With` meta) <$> runMessage msg attrs
   where
    placed n = pure $ Ruthless (attrs `With` meta {scheming = False, succeeded = succeeded meta || n > 0})
