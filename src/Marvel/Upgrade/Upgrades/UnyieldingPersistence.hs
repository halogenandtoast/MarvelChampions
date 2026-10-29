module Marvel.Upgrade.Upgrades.UnyieldingPersistence (unyieldingPersistence, UnyieldingPersistence (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Count
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Heroes.NebulaHero (techniqueResolved)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

unyieldingPersistence :: UpgradeCard UnyieldingPersistence
unyieldingPersistence = upgrade UnyieldingPersistence Cards.unyieldingPersistence

newtype UnyieldingPersistence = UnyieldingPersistence (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade UnyieldingPersistence where
  upgradeSpecialCanResolve (UnyieldingPersistence attrs) =
    selectNone (IdentityCharacters (IdentityWithId $ upgradeController attrs) <> CharacterWithStatus ToughStatus)

instance HasAbilities UnyieldingPersistence where
  getAbilities _ = []

instance HasModifiersFor UnyieldingPersistence where
  getModifiersFor _ (IdentityRef ident) (UnyieldingPersistence attrs)
    | ident == upgradeController attrs = do
        hero <- selectAny (IdentityWithId ident <> HeroIdentity)
        pure $ if hero then [ThwartModifier 1, AttackModifier 1, KeywordModifier Stalwart] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage UnyieldingPersistence where
  runMessage msg u@(UnyieldingPersistence attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      tough <- selectAny (IdentityCharacters (IdentityWithId ident) <> CharacterWithStatus ToughStatus)
      unless tough do
        becomeTough ident
        techniqueResolved ident (upgradeId attrs)
      pure u
    _ -> UnyieldingPersistence <$> runMessage msg attrs
   where ident = upgradeController attrs
