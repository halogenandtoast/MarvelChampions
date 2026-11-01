module Marvel.Minion.Minions.Daemos where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
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

daemos :: MinionCard Daemos
daemos = minion Daemos Cards.daemos (Sch 1) (Atk 3) (HP 6)

newtype Daemos = Daemos (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Daemos where
  getModifiersFor _ (MinionRef mid) _ = do
    inheritor <- selectAny (MinionWithId mid <> MinionWithTrait Inheritor)
    pure $ if inheritor then [KeywordModifier Stalwart] else []
  getModifiersFor _ _ _ = pure []

instance RunMessage Daemos where
  runMessage msg (Daemos attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      webWarrior <- selectAny (CharacterWithTrait WebWarrior)
      when webWarrior do
        characters <- filterM canBeStunned =<< selectList (CharacterControlledBy $ IdentityWithId you)
        unless (null characters) $ chooseOne you [targeting (characterRef character) (giveStatus character StunnedStatus) | character <- characters]
      Daemos <$> runMessage msg attrs
    _ -> Daemos <$> runMessage msg attrs

   where
    canBeStunned character = do
      stunned <- characterMatches (CharacterWithStatus StunnedStatus) character
      modifiers <- getModifiers (characterRef character)
      printed <- getsGame $ \g -> maybe [] (toList . cdKeywords) (abilityCardDef g $ characterRef character)
      pure $ not stunned && Stalwart `notElem` (printed <> [keyword | KeywordModifier keyword <- modifiers])
