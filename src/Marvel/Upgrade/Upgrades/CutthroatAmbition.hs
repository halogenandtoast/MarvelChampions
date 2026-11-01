module Marvel.Upgrade.Upgrades.CutthroatAmbition (cutthroatAmbition, CutthroatAmbition (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types ()
import Marvel.Count
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hero.Heroes.NebulaHero (techniqueResolved)
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

cutthroatAmbition :: UpgradeCard CutthroatAmbition
cutthroatAmbition = upgrade CutthroatAmbition Cards.cutthroatAmbition

newtype CutthroatAmbition = CutthroatAmbition (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade CutthroatAmbition where
  upgradeSpecialCanResolve (CutthroatAmbition attrs) = do
    let ident = upgradeController attrs
    canThwart <- selectAny (IdentityWithId ident <> IdentityCanThwart)
    confused <- selectAny (IdentityWithId ident <> ConfusedIdentity)
    targets <- cutthroatTargets attrs
    pure $ canThwart && (confused || not (null targets))

cutthroatTargets :: (HasGame m) => Attrs Upgrade -> m [SchemeId]
cutthroatTargets attrs = asYou (upgradeController attrs) do
  schemes <- selectList SchemeYouCanThwart
  filterM (\s -> do
    threat <- selectCount SchemeThreat (SchemeWithId s)
    modifiers <- getModifiers (schemeRef s)
    pure $ threat > 0 && ThreatCannotBeRemovedByThwarting `notElem` modifiers) schemes

instance HasAbilities CutthroatAmbition where
  getAbilities _ = []

instance HasModifiersFor CutthroatAmbition where
  getModifiersFor _ (IdentityRef ident) (CutthroatAmbition attrs)
    | ident == upgradeController attrs = do
        hero <- selectAny (IdentityWithId ident <> HeroIdentity)
        pure $ if hero then [KeywordModifier Piercing, KeywordModifier Overkill] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage CutthroatAmbition where
  runMessage msg u@(CutthroatAmbition attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      canThwart <- selectAny (IdentityWithId ident <> IdentityCanThwart)
      if not canThwart then pure u else thwartGuard u do
        eligible <- cutthroatTargets attrs
        unless (null eligible) $ chooseOne ident
          [targeting (schemeRef s) do
            thwartScheme s attrs 3
            techniqueResolved ident (upgradeId attrs)
          | s <- eligible]
        pure u
    _ -> CutthroatAmbition <$> runMessage msg attrs
