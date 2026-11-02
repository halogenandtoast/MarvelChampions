module Marvel.SideScheme.SideSchemes.ShowofEmpathyShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

showofEmpathyShuri :: SideSchemeCard ShowofEmpathyShuri
showofEmpathyShuri = sideScheme ShowofEmpathyShuri Cards.showofEmpathyShuri (Static 6)
newtype ShowofEmpathyShuri = ShowofEmpathyShuri (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ShowofEmpathyShuri where
  runMessage msg u@(ShowofEmpathyShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [n | W.ThreatRemovedBy _ (SchemeSideSchemeId sid) _ n <- windows, sid == sideSchemeId attrs] $ \n -> do
        minions <- selectList (NotMinion $ MinionWithTrait T.Elite)
        chooseOne you [targeting mid (placeCharacterThreat mid n >> cardSpecific attrs "redeem" (you,mid)) | mid <- minions]
      pure u
    CardSpecific (isTarget attrs -> True) "redeem" (fromJSON -> Success (you,mid :: MinionId)) -> do
      threat <- getCharacterThreat (MinionCharacter mid)
      hp <- getEnemyRemainingHitPoints (EnemyMinionId mid)
      when (threat >= hp) $ do
        addToVictoryDisplay attrs
        attachSetAsideUpgradeToMinion you "51036" mid
      pure u
    _ -> ShowofEmpathyShuri <$> runMessage msg attrs

instance HasAbilities ShowofEmpathyShuri where
  getAbilities u@(ShowofEmpathyShuri attrs) = [limitedWindowAbility u 1 (W.ThreatRemovedFromScheme $ SchemeWithId $ SchemeSideSchemeId $ sideSchemeId attrs) ForcedInterrupt (MinionExists $ NotMinion $ MinionWithTrait T.Elite) NoCost (runAbility u 1)]

instance HasModifiersFor ShowofEmpathyShuri where
  getModifiersFor _ target (ShowofEmpathyShuri attrs) | target == toTarget attrs = do
    minion <- selectAny (NotMinion $ MinionWithTrait T.Elite)
    pure [RemovedThreatPlacedOnMinion | minion]
  getModifiersFor _ _ _ = pure []

instance IsSideScheme ShowofEmpathyShuri where
  toSideSchemeAttrs (ShowofEmpathyShuri attrs) = attrs
  sideSchemeAbilities = getAbilities
