module Marvel.Treachery.Treacheries.MastersOfMayhem (
  mastersOfMayhem,
  MastersOfMayhem (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

mastersOfMayhem :: TreacheryCard MastersOfMayhem
mastersOfMayhem =
  treachery (MastersOfMayhem . (`With` Meta False)) Cards.mastersOfMayhem

newtype Meta = Meta {attackMade :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype MastersOfMayhem = MastersOfMayhem (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery MastersOfMayhem where
  toTreacheryAttrs (MastersOfMayhem (attrs `With` _)) = attrs

instance RunMessage MastersOfMayhem where
  runMessage msg t@(MastersOfMayhem (attrs `With` meta)) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        mastersOfEvilMinions <- selectList (MinionWithTrait MastersOfEvil)
        for_ mastersOfEvilMinions $ \minion -> do
          mEngagedWith <- selectOne (IdentityEngagedWith $ MinionWithId minion)
          case mEngagedWith of
            Nothing -> pure ()
            Just engagedWith ->
              push (MinionMessage minion $ MinionAttacks engagedWith)
        pure t
      CheckTreacheryCondition identityId -> do
        unless (attackMade meta) $
          push $
            IdentityMessage identityId $
              Search
                SearchEncounterDeckAndDiscardPile
                (CardWithType MinionType <> CardWithTrait MastersOfEvil)
                (SearchTarget $ toTarget attrs)
                ShuffleBackIn

        pure t
      _ -> MastersOfMayhem . (`With` meta) <$> runMessage msg attrs
    MinionMessage mid MinionAttacked -> do
      mastersOfEvilMinion <- minionMatches (MinionWithTrait MastersOfEvil) mid
      pure $
        MastersOfMayhem
          ( attrs
              `With` meta {attackMade = mastersOfEvilMinion || attackMade meta}
          )
    _ -> MastersOfMayhem . (`With` meta) <$> runMessage msg attrs
