module Marvel.Upgrade.Upgrades.ClarityOfPurpose where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Cost
import Marvel.Id
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Payment
import Marvel.Question (Choice (Pay))
import Marvel.Resource

clarityOfPurpose :: UpgradeCard ClarityOfPurpose
clarityOfPurpose = upgrade ClarityOfPurpose Cards.clarityOfPurpose

newtype ClarityOfPurpose = ClarityOfPurpose (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade ClarityOfPurpose where
  upgradeTargets _ = do
    identities <- selectList AnyIdentity
    openIdentities <- filterM (\p -> selectNone (UpgradeIs Cards.clarityOfPurpose <> UpgradeOnIdentity (IdentityWithId p))) identities
    allies <- selectList (NotAlly $ AllyWithUpgrade $ UpgradeIs Cards.clarityOfPurpose)
    pure $ Just (map toTarget openIdentities <> map toTarget allies)

clarityHost :: Attrs Upgrade -> CharacterId
clarityHost attrs = maybe (IdentityCharacter $ upgradeController attrs) AllyCharacter (upgradeAttachedAlly attrs)

instance HasAbilities ClarityOfPurpose where
  getAbilities a@(ClarityOfPurpose attrs) = [ability a 1 HeroResource OwnsThis
    (ExhaustCost <> DamageCharacterCost (CharacterWithId $ clarityHost attrs) 1) (Pay $ ResourcePayment Wild)]

instance RunMessage ClarityOfPurpose where
  runMessage msg (ClarityOfPurpose attrs) = ClarityOfPurpose <$> runMessage msg attrs
