module Marvel.Upgrade.Upgrades.GitGud where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Card.Id (CardId (..))
import Marvel.Id (UpgradeId (..))
import Marvel.Window qualified as W

gitGud :: UpgradeCard GitGud
gitGud = upgrade GitGud Cards.gitGud

newtype GitGud = GitGud (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor GitGud where
  getModifiersFor _ (CardIdRef cid) (GitGud attrs)
    | cid == CardId (unUpgradeId $ upgradeId attrs) = do
        won <- getPreviousGameWon (upgradeController attrs)
        pure [ResourceCostReduction 2 | won /= Just True]
  getModifiersFor _ _ _ = pure []

instance HasAbilities GitGud where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.IdentityWouldBeDefeated AnyIdentity)
        ForcedInterrupt OwnsThis NoCost (runAbility a 1)
    ]

instance RunMessage GitGud where
  runMessage msg u@(GitGud attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      victims <- filterM (\victim -> isQueued (== IdentityMessage victim IdentityDefeated))
        (nub [victim | W.IdentityDefeat victim <- windows])
      unless (null victims) $ chooseOrRunOne you
        [ targeting victim do
            cancelMatchingMessage (== IdentityMessage victim IdentityDefeated)
            setHitPoints victim 1
            changeToAlterEgo victim
            removeFromGame attrs
        | victim <- victims
        ]
      pure u
    _ -> GitGud <$> runMessage msg attrs
