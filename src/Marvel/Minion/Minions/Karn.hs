module Marvel.Minion.Minions.Karn where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Card.Code ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Status ()
import Marvel.Trait (Trait (Inheritor, WebWarrior))
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

karn :: MinionCard Karn
karn = minion Karn Cards.karn (Sch 1) (Atk 3) (HP 5)

newtype Karn = Karn (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Karn where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [KeywordModifier Overkill, KeywordModifier Piercing] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Karn where
  runMessage msg (Karn attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
        supports <- selectList (SupportControlledBy $ IdentityWithId you)
        let targets = map UpgradeRef upgrades <> map SupportRef supports
        legal <- filterM (\target -> do
          modifiers <- getModifiers target
          def <- getsGame (`abilityCardDef` target)
          pure $ CannotBeDiscarded `notElem` modifiers && maybe True (notElem Permanent . cdKeywords) def) targets
        chooseOne you [targeting target (removeFromPlay target) | target <- legal]
      Karn <$> runMessage msg attrs
    _ -> Karn <$> runMessage msg attrs
