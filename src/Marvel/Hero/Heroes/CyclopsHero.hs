module Marvel.Hero.Heroes.CyclopsHero where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Window qualified as W
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Ability.Types qualified as Ability

cyclopsHero :: HeroCard CyclopsHero
cyclopsHero = hero (CyclopsHero . (`With` emptyBlast)) Cards.cyclopsHero (HP $ Static 10) (HandSize 5) (Thw 2) (Atk 1) (Def 2)

-- Only Optic Blast reads these flags; nested attacks cannot inherit them.
data Blast = Blast {visorPayment :: Maybe ActiveCostId, visorUsed :: Bool, fullBlastUsed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyBlast :: Blast
emptyBlast = Blast Nothing False False

newtype CyclopsHero = CyclopsHero (Attrs Hero `With` Blast)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsHero CyclopsHero where
  toHeroAttrs (CyclopsHero (attrs `With` _)) = attrs

instance IsRef CyclopsHero where
  toRef = toRef . toHeroAttrs

instance HasModifiersFor CyclopsHero

instance HasAbilities CyclopsHero where
  getAbilities a =
    [ (label "$label.opticBlast" $ limitedAbility a 1 (PerRound 1) Action
        (IsSelf <> SelfMatches IdentityCanAttack <> AnyCriteria [SelfMatches StunnedIdentity, EnemyExists (AttackableEnemy <> EnemyWithUpgrade (UpgradeMatches []))])
        (ResourceCost Nothing) (runAbility a 1)) {abilitySubType = Just Ability.Attack}
    ]

instance RunMessage CyclopsHero where
  runMessage msg (CyclopsHero (attrs `With` blast)) = runQueueT $ case msg of
    CardSpecific (isTarget attrs -> True) "cyclopsVisor" (fromJSON -> Success scope) ->
      pure . CyclopsHero $ attrs `With` blast {visorPayment = scope, visorUsed = True}
    CardSpecific (isTarget attrs -> True) "cyclopsFullBlast" _ ->
      pure . CyclopsHero $ attrs `With` blast {fullBlastUsed = True}
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList (AttackableBy (IdentityWithId you) <> EnemyWithUpgrade (UpgradeMatches []))
      let amount = if fullBlastUsed blast then 11 else 3
          damage = (toDamage amount $ FromPlayerAttack you)
            {damageOverkill = fullBlastUsed blast, damagePiercing = visorUsed blast, damageRanged = visorUsed blast}
      unless (null enemies) $ chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs damage) | enemy <- enemies]
      pure . CyclopsHero $ attrs `With` emptyBlast
    AbortPayment acid | visorPayment blast == Just acid -> pure . CyclopsHero $ attrs `With` emptyBlast
    CardSpecific GameRef "resolveLabeledAbility" (fromJSON -> Success (you, ab, _windows :: [W.WindowType], _payment :: Payment))
      | you == heroIdentityId attrs, abilitySource ab == toSource attrs, abilityIndex ab == 1 -> do
          canceled <- identityMatches StunnedIdentity you
          pure . CyclopsHero $ attrs `With` (if canceled then emptyBlast else blast)
    EndRound -> do
      attrs' <- runMessage msg attrs
      pure . CyclopsHero $ attrs' `With` emptyBlast
    _ -> CyclopsHero . (`With` blast) <$> runMessage msg attrs
