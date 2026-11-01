module Marvel.Upgrade.Upgrades.PsiKatana (psiKatana) where
import Marvel.Upgrade.Upgrades.PsiKnife (PsiKnife (..))
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards
psiKatana :: UpgradeCard PsiKnife
psiKatana = upgrade PsiKnife Cards.psiKatana
