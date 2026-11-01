module Marvel.Iceman.Helpers where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity ()
import Marvel.Damage (Damage)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Window qualified as W

-- Set-aside Frostbites are a shared physical supply. A copied Iceman event may
-- attach one even when the player resolving that event is not Iceman.
attachFrostbite :: ReverseQueue m => IdentityId -> EnemyId -> m ()
attachFrostbite you enemy = do
  exists <- selectAny (EnemyWithId enemy)
  cards <- filter ((== "46002") . toCardCode) <$> getSetAsidePlayerCards
  when exists $ for_ (headMay cards) $ \card -> putIntoPlayOn you card (enemyRef enemy)

chooseFrostbite :: ReverseQueue m => IdentityId -> m ()
chooseFrostbite you = do
  cards <- filter ((== "46002") . toCardCode) <$> getSetAsidePlayerCards
  unless (null cards) $ do
    enemies <- selectList AnyEnemy
    chooseOne you [targeting (enemyRef enemy) $ attachFrostbite you enemy | enemy <- enemies]

frostbitten :: EnemyMatcher
frostbitten = EnemyWithUpgrade (UpgradeIs Cards.frostbite)

-- The breaking wall attaches its Frostbite after the activation's response window:
-- existing Frostbites have melted first (official Ice Wall ruling, 30 May 2024).
attachFrostbiteAfterActivation :: ReverseQueue m => IdentityId -> EnemyId -> m ()
attachFrostbiteAfterActivation you enemy =
  afterMatching (\case
    CheckWindows windows -> any (\case
      W.Window W.After (W.EnemyActivation e _) -> e == enemy
      _ -> False) windows
    _ -> False) $ cardSpecific GameRef "icemanDelayedFrostbite" (you, enemy)

-- The first Arctic Attack option resolves its attachment and damage together.
-- The game defers consequences of both until the immediate changes have happened;
-- the attachment's continuation precedes defeat, even for a lethal hit.
attachFrostbiteAndDamage :: (ReverseQueue m, IsRef source) => IdentityId -> EnemyId -> source -> Damage -> m ()
attachFrostbiteAndDamage you enemy source damage = do
  attachment <- capture $ attachFrostbite you enemy
  dealt <- capture $ dealDamage enemy source damage
  let (before, now, after) = splitDamage dealt
  enqueueContinuation before
  enqueueContinuation [simultaneously $ attachment <> now]
  enqueueContinuation after
