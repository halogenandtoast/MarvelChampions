module Marvel.SideScheme.SideSchemes.SmVenom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Environments.SmVenom (bellTowers)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Villain.Cards qualified as Villains
import Marvel.Window
import Marvel.Window qualified as W

newtype SmVenomSideScheme = SmVenomSideScheme (Attrs SideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme SmVenomSideScheme where
  sideSchemeAbilities (SmVenomSideScheme a)
    | toCardCode a `elem` ["27080", "27081"], sideSchemeThreat a > 0 =
        [windowAbility a 1 (WindowOneOf [EnemyTookDamage (EnemyIs def) AnyAttack | def <- [Villains.smVenom1, Villains.smVenom2, Villains.smVenom3]]) Response NoCost (runAbility a 1)]
    | otherwise = []

instance HasModifiersFor SmVenomSideScheme where
  getModifiersFor _ target (SmVenomSideScheme a)
    | toCardCode a == "27079", EnvironmentRef _ <- target = do
        def <- getsGame (`abilityCardDef` target)
        pure [Blank | maybe False ((== "27077") . toBaseCardCode . cdCardCode) def]
  getModifiersFor _ _ _ = pure []

smGuardTheBellTower, smLashingOut, smToothAndNail :: SideSchemeCard SmVenomSideScheme
smGuardTheBellTower = sideScheme SmVenomSideScheme Cards.smGuardTheBellTower (Static 3)
smLashingOut = sideScheme SmVenomSideScheme Cards.smLashingOut (Static 9)
smToothAndNail = sideScheme SmVenomSideScheme Cards.smToothAndNail (Static 8)

instance RunMessage SmVenomSideScheme where
  runMessage msg e@(SmVenomSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs, toCardCode attrs == "27079" -> do
      towers <- bellTowers
      for_ towers \tower -> cardSpecific tower "resetQuiet" ()
      pure e
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [damageAmount damage | W.EnemyTookDamageFrom _ damage _ <- windows] \n ->
        removeThreat you attrs n (SchemeWithId $ SchemeSideSchemeId $ sideSchemeId attrs)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      attacks <- selectAny (EnemyWithId enemy <> AttackingEnemy)
      when attacks $ case toCardCode attrs of
        "27080" -> do
          you <- getBoostedPlayer enemy
          dealIndirectDamage you attrs 2
        "27081" -> attackGains enemy Piercing
        _ -> pure ()
      pure e
    _ -> SmVenomSideScheme <$> runMessage msg attrs
