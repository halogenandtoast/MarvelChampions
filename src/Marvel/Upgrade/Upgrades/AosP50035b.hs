{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Upgrade.Upgrades.AosP50035b where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

aosP50035b :: UpgradeCard AosP50035b
aosP50035b = upgrade AosP50035b Cards.aosP50035b

newtype AosP50035b = AosP50035b (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50035b where
  getAbilities (AosP50035b attrs) =
    [ limitedWindowAbility attrs 1 (W.IdentityAttacked W.When (IdentityWithId $ upgradeController attrs) AnyEnemy) Interrupt (OwnsThis <> InHeroForm) NoCost (runAbility attrs 1)
    | toCardCode attrs == "50035a"] <>
    [ limitedWindowAbility attrs 2 (W.EnemyWouldAttack AnyEnemy (IdentityWithId $ upgradeController attrs)) ForcedInterrupt (OwnsThis <> InHeroForm) NoCost (runAbility attrs 2)
    | toCardCode attrs == "50035b"] <>
    [ limitedWindowAbility attrs 3 (W.ThreatWouldBePlaced threatSource MainScheme) ForcedInterrupt OwnsThis NoCost (runAbility attrs 3)
    | upgradeRedirectNextScheme attrs, upgradeUses attrs <= 5, threatSource <- [W.ThreatFromVillain,W.ThreatFromMinion]]

instance RunMessage AosP50035b where
  runMessage msg card@(AosP50035b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you [labeled (countLabel "aosSpendThreat" n) do
        removeCounters attrs n
        modifyCurrentPlayerAttack you n [] | n <- [0..min 3 $ upgradeUses attrs]]
      pure card
    RanAbility you (isTarget attrs -> True) 2 windows _ -> do
      for_ [enemy | W.EnemyAttack enemy target <- windows,target == you] $ \enemy -> do
        replacePendingAttackWithScheme enemy you
        cardSpecific attrs "clearSchemeRedirect" ()
      pure $ AosP50035b attrs {upgradeRedirectNextScheme = True}
    RanAbility _ (isTarget attrs -> True) 3 windows _ -> do
      for_ [(mid,n) | W.ThreatPlaced _ (SchemeMainSchemeId mid) n <- windows,n>0] $ \(mid,_) -> do
        replaceMatchingMessage (\case
          MainSchemeMessage ident (MainSchemePlaceThreat n) -> [MainSchemeMessage ident (MainSchemePlaceThreat (n-1)) | n>1]
          other -> [other]) (\case MainSchemeMessage ident (MainSchemePlaceThreat _) -> ident == mid; _ -> False)
        placeCounters attrs 1
      pure $ AosP50035b attrs {upgradeRedirectNextScheme = False}
    CheckWindows windows | any (\w -> case W.windowType w of W.EnemyActivation _ who -> W.windowTiming w == W.After && who == upgradeController attrs; _ -> False) windows ->
      pure $ AosP50035b attrs {upgradeRedirectNextScheme = False}
    CardSpecific (isTarget attrs -> True) "clearSchemeRedirect" _ -> pure $ AosP50035b attrs {upgradeRedirectNextScheme = False}
    CardSpecific (isTarget attrs -> True) "changeSuit" (fromJSON -> Success stealth) -> do
      let def = if stealth then Cards.aosP50035b else Cards.aosP50035a
      when (toCardCode attrs /= toCardCode def) $ checkWindows [W.Window W.After $ W.SuitFormChanged (upgradeController attrs) (upgradeId attrs)]
      pure $ AosP50035b attrs {upgradeCardDef = def}
    CardSpecific (isTarget attrs -> True) "suitThreat" (fromJSON -> Success (add,n :: Natural)) ->
      pure $ AosP50035b attrs {upgradeUses = if add then upgradeUses attrs+n else subtractNatural n (upgradeUses attrs)}
    _ -> AosP50035b <$> runMessage msg attrs
