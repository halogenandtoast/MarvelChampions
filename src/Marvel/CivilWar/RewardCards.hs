module Marvel.CivilWar.RewardCards where

import Marvel.Prelude
import Marvel.Ability.Types (AbilityType (Action,Interrupt))
import Marvel.Aspect (Aspect (Neutral))
import Marvel.Card
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Criteria
import Marvel.Name (Name)
import Marvel.Matchers
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.Resource.Types

rewardEvent :: CardCode -> Name -> Int -> Maybe CardCode -> CardDef
rewardEvent code name cost leader = CardDef
  { cdCardCode = code,cdName = name,cdCost = Just cost
  , cdTraits = mempty,cdKeywords = mempty,cdCardType = EventType
  , cdAbilityType = Just Action,cdAbilitySubTypes = [],cdUnique = False
  , cdAspect = Just Neutral,cdCriteria = FriendlyLeaderExists leader
  , cdResources = [(PrintedResource,Wild)],cdResponseWindow = Nothing
  , cdBoostIcons = [],cdEncounterSet = Nothing,cdEncounterSetQuantity = Nothing
  , cdActionCriteria = Nothing,cdHazards = 0,cdAcceleration = 0,cdAmplify = 0
  , cdLimit = Nothing,cdDeckLimit = Nothing,cdTeamUp = Nothing
  , cdAlternateCardCodes = [],cdPlayableFromDiscard = False,cdAbilityCost = NoCost
  , cdDoubleWildResources = False,cdDoesNotCountTowardHandSize = False
  , cdCanPayForOthersWithTrait = Nothing
  }

rewardEvents :: [CardDef]
rewardEvents =
  [(rewardEvent "57032" "You're Out of Order!" 1 Nothing) {cdCriteria = NoCriteria,cdAbilityType = Just Interrupt,cdResponseWindow = Just $ W.ThreatWouldBePlaced W.AnyThreatSource EncounterMainScheme}
  ,rewardEvent "57033" "She-Hulk Smash" 2 (Just "57001")
  ,rewardEvent "57074" "Calculated Risk" 2 Nothing
  ,rewardEvent "57075" "Synthezoid Strike" 2 (Just "57040")
  ,(rewardEvent "57076" "Phased Out" 0 Nothing) {cdCriteria = AttachmentExists phasedOutAttachments}
  ,rewardEvent "56129" "The Futurist" 2 (Just "56059")
  ,rewardEvent "56130" "Target Lock" 2 (Just "56059")
  ,rewardEvent "56134" "You Started This" 2 (Just "56092")
  ,rewardEvent "56208" "Fighting Dirty" 2 (Just "56137")
  ,(rewardEvent "56209" "Recruited by Cap" 3 Nothing) {cdCriteria = FriendlyLeaderExists Nothing <> MinionExists (MinionEngagedWith You)}
  ,rewardEvent "56211" "Double Agent" 2 Nothing
  ,rewardEvent "56212" "Spider-Blast" 2 (Just "56168")]


rewardResources :: [CardDef]
rewardResources = [resource "57035" "Legal Counsel",resource "57077" "Android Assistance",resource "56132" "Suit Up",resource "56136" "Raw Power",resource "56210" "The People's Hero",resource "56214" "Secret Contact"]
 where
  resource code name = (rewardEvent code name 0 Nothing)
    {cdCost = Nothing,cdCardType = ResourceType,cdAbilityType = Nothing
    ,cdCriteria = NoCriteria,cdResources = [(PrintedResource,Wild),(PrintedResource,Wild)]}


rewardUpgrades :: [CardDef]
rewardUpgrades = makeMeAngry :
  [(rewardEvent "56131" "High-Tech Suit" 1 (Just "56059"))
    {cdCardType = UpgradeType,cdAbilityType = Nothing}
  ,(rewardEvent "56133" "Provoked Response" 0 (Just "56092"))
    {cdCardType = UpgradeType,cdAbilityType = Nothing}
  ,(rewardEvent "56207" "No, You Move!" 0 (Just "56137"))
    {cdCardType = UpgradeType,cdAbilityType = Nothing}
  ,(rewardEvent "56135" "Alpha Flight Recruit" 0 Nothing)
    {cdCardType = UpgradeType,cdAbilityType = Nothing,cdCriteria = AllyExists AnyAlly,cdTraits = fromList [T.Title]}
  ,(rewardEvent "56213" "Hard to Hit" 1 (Just "56168"))
    {cdCardType = UpgradeType,cdAbilityType = Nothing,cdTraits = fromList [T.Skill]}]

phasedOutAttachments :: AttachmentMatcher
phasedOutAttachments = AttachmentOnVillain ActiveVillain <> AttachmentWithPrintedHeroActionOrResponse <> DiscardableAttachment

makeMeAngry :: CardDef
makeMeAngry = (rewardEvent "57034" "Make Me Angry" 1 (Just "57001")) {cdCardType = UpgradeType,cdAbilityType = Nothing}
