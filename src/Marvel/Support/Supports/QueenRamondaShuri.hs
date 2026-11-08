module Marvel.Support.Supports.QueenRamondaShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()
import Marvel.AlterEgo.Runner (getModifiedRecovery)
import Marvel.Identity.Types (Field(PlayerIdentitySide,PlayerIdentitySides), PlayerIdentitySide(..))
import Marvel.Projection

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Support.Types

queenRamondaShuri :: SupportCard QueenRamondaShuri
queenRamondaShuri = support QueenRamondaShuri Cards.queenRamondaShuri
newtype QueenRamondaShuri = QueenRamondaShuri (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage QueenRamondaShuri where
  runMessage msg u@(QueenRamondaShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- selectList (AlterEgoIdentity <> IdentityWithTrait T.Wakanda <> recoveryHealableIdentity)
      chooseOne you [targeting player (cardSpecific attrs "heal" player) | player <- players]
      pure u
    CardSpecific (isTarget attrs -> True) "heal" (fromJSON -> Success (you :: IdentityId)) -> do
      side <- project PlayerIdentitySide you
      sides <- project PlayerIdentitySides you
      case lookup side sides of
        Just (AlterEgoSide ae) -> heal you =<< getModifiedRecovery (toAttrs ae)
        _ -> pure ()
      pure u
    _ -> QueenRamondaShuri <$> runMessage msg attrs

instance HasAbilities QueenRamondaShuri where
  getAbilities u@(QueenRamondaShuri _attrs) = [ability u 1 AlterEgoAction (OwnsThis <> CharacterExists (IdentityCharacters (AlterEgoIdentity <> IdentityWithTrait T.Wakanda <> recoveryHealableIdentity))) ExhaustCost (runAbility u 1)]
