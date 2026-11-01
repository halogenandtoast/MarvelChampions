module Marvel.Minion.Minions.GuildAssassin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

guildAssassin :: MinionCard GuildAssassin
guildAssassin = minion GuildAssassin Cards.guildAssassin (Sch 1) (Atk 2) (HP 2)

newtype GuildAssassin = GuildAssassin (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GuildAssassin where
  getAbilities (GuildAssassin attrs) =
    [windowAbility attrs 1 (W.CharacterDefeatedByEnemyAttack W.After $ EnemyWithId $ EnemyMinionId $ minionId attrs)
      ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage GuildAssassin where
  runMessage msg m@(GuildAssassin attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      placeThreatOnEach attrs 1 EncounterMainScheme
      pure m
    _ -> GuildAssassin <$> runMessage msg attrs
