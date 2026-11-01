module Marvel.Ally.Allies.ProfessorX where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

professorX :: AllyCard ProfessorX
professorX = ally ProfessorX Cards.professorX (Thw 3, 1) (Atk 0, 1) (HP 3)

newtype ProfessorX = ProfessorX (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ProfessorX where
  getAbilities (ProfessorX a) =
    [windowAbility a 1 W.EntersPlayThis ForcedResponse NoCost (runAbility a 1)
    , windowAbility a 2 W.RoundEnds ForcedInterrupt NoCost (runAbility a 2)
    ]

instance RunMessage ProfessorX where
  runMessage msg e@(ProfessorX attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      villains <- selectList ActiveVillain >>= filterM (enemyMatches EnemyCanBeConfused . EnemyVillainId)
      minions <- selectList AnyMinion >>= filterM (enemyMatches EnemyCanBeStunned . EnemyMinionId)
      characters <- selectList (CharacterWithTrait Trait.XMen <> CharacterCanBeReadied)
      let choices =
            [targeting vid (confuse vid attrs) | vid <- villains]
            <> [targeting mid (stun mid attrs) | mid <- minions]
            <> [targeting (characterRef character) (case character of IdentityCharacter p -> ready p; AllyCharacter aid -> ready aid; _ -> pure ()) | character <- characters]
      unless (null choices) $ chooseOne you choices
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> e <$ removeFromPlay attrs
    _ -> ProfessorX <$> runMessage msg attrs
