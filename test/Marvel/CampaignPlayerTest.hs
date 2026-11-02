module Marvel.CampaignPlayerTest where

import Data.List (find)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying you code resources = do
  card <- putInHand you code
  payments <- traverse (putInHand you) resources
  playWith card payments
  pure card

heroPowers :: IdentityId -> GameAppT (Natural,Natural,Natural)
heroPowers you = currentIdentity <$> getPlayer you >>= \case
  HeroSide hero -> (,,) <$> Hero.getModifiedThwart (toAttrs hero) <*> Hero.getModifiedAttack (toAttrs hero) <*> Hero.getModifiedDefense (toAttrs hero)
  _ -> pure (0,0,0)

basicAttack :: IdentityId -> Choice -> Bool
basicAttack you = \case UseAbility ab -> abilitySource ab == toSource you && abilityIndex ab == 301; _ -> False

basicThwart :: IdentityId -> Choice -> Bool
basicThwart you = \case UseAbility ab -> abilitySource ab == toSource you && abilityIndex ab == 300; _ -> False

discardCodes :: IdentityId -> GameAppT [CardCode]
discardCodes you = map toCardCode . unDiscard . playerIdentityDiscard . toAttrs <$> getPlayer you

reachPlayWindow :: PlayerCard -> GameAppT ()
reachPlayWindow card = go (20 :: Int)
 where
  go 0 = error "did not reach the expected event opportunity"
  go n = do
    playable <- offers (isPlay card)
    unless playable do
      question <- asked
      case question of
        Just (_, choices) -> case find canPass choices of
          Just choice -> choose "decline the preceding window" (== choice) >> go (n-1)
          Nothing -> error "unexpected question before the event opportunity"
        _ -> error "no question before the event opportunity"
  canPass choice = any (`isLabel` choice) ["$label.noResponse","$label.noDefender","$label.declineToDefend"]

-- A replacement applied in the scheme's real "when thwarted" window, after the
-- player has chosen the target but before any threat actually leaves it.
reducePendingThwart :: Target -> Natural -> GameAppT ()
reducePendingThwart target reduction = do
  present <- isQueued matches
  unless present $ error "expected the targeted thwart continuation"
  replaceMatchingMessage change matches
 where
  matches = \case CardSpecific selected "resolveThwart" _ -> selected == target; _ -> False
  change = \case
    CardSpecific selected "resolveThwart" (fromJSON -> Success (source :: Source,amount :: Natural)) ->
      [CardSpecific selected "resolveThwart" $ toJSON (source,subtractNatural reduction amount)]
    other -> [other]

-- Reach the real villain attack's defender choice, preserving response windows.
toVillainAttack :: IdentityId -> GameAppT ()
toVillainAttack _ = endTurn >> untilDefend (20 :: Int)
 where
  untilDefend 0 = error "the villain did not attack"
  untilDefend remaining = whenM (not <$> offers isDefend) do
    choose "decline the preceding response" (isLabel "$label.noResponse")
    untilDefend (remaining - 1)

-- Printed card abilities use low indices; basic ally powers share the source.
cardAbility :: PlayerCard -> Choice -> Bool
cardAbility card choice = isAbilityOfCard card choice && case choice of
  UseAbility ability' -> abilityIndex ability' == 1
  _ -> False
