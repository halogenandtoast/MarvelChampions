module Marvel.Treachery.Treacheries.ISeeYou (
  iSeeYou,
  ISeeYou (..),
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
import Marvel.Game.Source
import Marvel.Id
import Marvel.Modifier
import Marvel.Trait (Trait (Goblin))

iSeeYou :: TreacheryCard ISeeYou
iSeeYou = treachery (ISeeYou . (`With` Meta False)) Cards.iSeeYou

newtype Meta = Meta {withoutBoost :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ISeeYou = ISeeYou (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery ISeeYou where
  toTreacheryAttrs (ISeeYou (attrs `With` _)) = attrs
  treacheryModifiersFor _ (VillainRef _) (ISeeYou (_ `With` meta)) = pure [NoActivationBoostCard | withoutBoost meta]
  treacheryModifiersFor _ _ _ = pure []

instance RunMessage ISeeYou where
  runMessage msg t@(ISeeYou (attrs `With` meta)) = runQueueT $ case msg of
    -- When Revealed: Green Goblin attacks you; in alter-ego form, he is given no boost
    -- card for it
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      villain <- selectJust ActiveVillain
      alterEgo <- identityMatches AlterEgoIdentity you
      enemyAttacks villain you
      pure $ ISeeYou (attrs `With` Meta alterEgo)
    -- Boost: +1 boost icon if a Goblin minion is engaged with you
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getActivePlayerId
      goblins <- selectAny (MinionWithTrait Goblin <> MinionEngagedWith (IdentityWithId you))
      when goblins case enemy of
        EnemyVillainId vid -> addBoost vid 1
        EnemyMinionId mid -> minionStep mid (MinionAddBoost 1)
      pure t
    _ -> ISeeYou . (`With` meta) <$> runMessage msg attrs
