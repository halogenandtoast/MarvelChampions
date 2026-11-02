module Marvel.BlackPantherShuri.Helpers where
import Marvel.Prelude
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Trait
import Marvel.Query
import Marvel.Id

choosePantherSpecial :: ReverseQueue m => IdentityId -> m ()
choosePantherSpecial you = do
  upgrades <- selectList (UpgradeWithTrait BlackPanther <> UpgradeControlledBy (IdentityWithId you) <> UpgradeWithResolvableSpecial)
  chooseOnePrompted you "$label.chooseSpecial" [targetingAbility uid 1 | uid <- upgrades]

resolvePantherSpecials :: ReverseQueue m => IdentityId -> m ()
resolvePantherSpecials you = do
  upgrades <- selectList (UpgradeWithTrait BlackPanther <> UpgradeControlledBy (IdentityWithId you) <> UpgradeWithResolvableSpecial)
  chooseOneAtATimePrompted you "$label.chooseSpecial" [targetingAbility uid 1 | uid <- upgrades]
