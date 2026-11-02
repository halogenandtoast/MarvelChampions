{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AgentsOfShield.Helpers where

import Marvel.Prelude
import Marvel.Question (Choice)
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Game.Source
import Marvel.Hood.Helpers (activationWillResolve)
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref

activateMinion :: ReverseQueue m => IdentityId -> MinionId -> m Bool
activateMinion you mid = do
  resolves <- activationWillResolve you (EnemyMinionId mid)
  hero <- identityMatches HeroIdentity you
  if hero then enemyAttacks mid you else minionSchemesAgainst mid you
  pure resolves

-- Finding an enemy already in play changes its engagement without revealing it again.
findMinion :: (ReverseQueue m, IsRef host) => IdentityId -> CardDef -> host -> m ()
findMinion you def host = do
  found <- selectList (MinionIs def)
  if null found then searchEverywhereAndRevealEncounterCard you (CardIs def)
  else traverse_ (\mid -> engageMinion mid you) found
  cardSpecific host "aosActivateFound" (you, def)

activateFound :: ReverseQueue m => IdentityId -> CardDef -> m Bool
activateFound you def = do
  found <- selectList (MinionIs def)
  or <$> traverse (activateMinion you) found

rotateMinions :: ReverseQueue m => m ()
rotateMinions = do
  players <- getPlayers
  let next = drop 1 players <> take 1 players
  engagements <- for (zip players next) $ \(you,clockwise) -> do
    mids <- selectList (MinionEngagedWith $ IdentityWithId clockwise)
    pure (you,mids)
  for_ engagements $ \(you,mids) -> traverse_ (\mid -> engageMinion mid you) mids

chooseCharacterWithMostHP :: ReverseQueue m => IdentityId -> [CharacterId] -> (CharacterId -> m ()) -> m ()
chooseCharacterWithMostHP you chars body = do
  scored <- traverse (\c -> (c,) <$> getCharacterRemainingHitPoints c) chars
  unless (null scored) $ chooseOrRunOne you
    [targeting (characterRef c) (body c) | (c,hp) <- scored, hp == maximum (map snd scored)]

searchAndAttachEncounterCard :: (ReverseQueue m, IsEnemy enemy) => IdentityId -> CardMatcher -> enemy -> m Bool
searchAndAttachEncounterCard you matcher enemy = do
  cards <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
  let found = filter (cardMatch matcher) cards
  unless (null found) $ chooseOrRunOne you [targeting card (attachEncounterCard card enemy) | card <- found]
  shuffleEncounterDeck
  pure $ notNull found

payThen :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Cost -> m () -> m ()
payThen you source cost body = capture body >>= payCostThen you source cost

-- Mandatory effects resolve as far as possible when no legal choice exists.
chooseEffect :: ReverseQueue m => IdentityId -> [m Choice] -> m ()
chooseEffect you choices = do
  game <- getGame
  legal <- filter (offerable game) <$> sequence choices
  unless (null legal) $ chooseOrRunOne you (map pure legal)
