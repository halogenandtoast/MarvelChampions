{-# OPTIONS_GHC -Wno-orphans #-}

module Marvel.Attachment where

import Marvel.Attachment.Attachments.TheHood qualified as Hood
import Marvel.Prelude

import Marvel.Attachment.Attachments
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Builder (reprintAs)
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Id

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o -> do
    cardDef <- o .: "attachmentCardDef"
    withAttachmentCardCode (cdCardCode cardDef) $
      \(_ :: AttachmentCard a) -> Attachment <$> parseJSON @a (Object o)

withAttachmentCardCode ::
  CardCode -> (forall a. (IsAttachment a) => AttachmentCard a -> r) -> r
withAttachmentCardCode cCode f = case lookup cCode allAttachments of
  Nothing -> error "invalid attachment"
  Just (SomeAttachmentCard a) -> f a

allAttachments :: HashMap CardCode SomeAttachmentCard
allAttachments =
  fromList $
    map
      (toFst someAttachmentCardCode)
      [ SomeAttachmentCard Hood.establishedDominance
      , SomeAttachmentCard Hood.theHoodsMantle
      , SomeAttachmentCard Hood.theHoodsPistol
      , SomeAttachmentCard armoredRhinoSuit
      , SomeAttachmentCard charge
      , SomeAttachmentCard enhancedIvoryHorn
      , SomeAttachmentCard sonicConverter
      , SomeAttachmentCard solidSoundBody
      , SomeAttachmentCard programTransmitter
      , SomeAttachmentCard upgradedDrones
      , SomeAttachmentCard vibraniumArmor
      , SomeAttachmentCard concussionBlasters
      , SomeAttachmentCard geneticallyEnhanced
      , SomeAttachmentCard biomechanicalUpgrades
      , SomeAttachmentCard counterspell
      -- The Green Goblin
      , SomeAttachmentCard goblinGlider
      , SomeAttachmentCard pumpkinBombs
      , SomeAttachmentCard allTiedUp
      , SomeAttachmentCard mediaCoverage
      , SomeAttachmentCard (reprintAs Cards.goblinGliderMutagenFormula goblinGlider)
      , SomeAttachmentCard hysteria
      , SomeAttachmentCard (reprintAs Cards.pumpkinBombsMutagenFormula pumpkinBombs)

      -- The Wrecking Crew
      , SomeAttachmentCard heldHostage
      , SomeAttachmentCard (reprintAs Cards.heldHostageThunderball heldHostage)
      , SomeAttachmentCard (reprintAs Cards.heldHostagePiledriver heldHostage)
      , SomeAttachmentCard (reprintAs Cards.heldHostageBulldozer heldHostage)
      , SomeAttachmentCard magicCrowbar
      , SomeAttachmentCard wreckersCommand
      , SomeAttachmentCard ballAndChain
      , SomeAttachmentCard radioactiveBuildup
      , SomeAttachmentCard distractingTaunts
      , SomeAttachmentCard bulldozersHelmet
      , SomeAttachmentCard rammingSpeed

      -- The Rise of Red Skull
      , SomeAttachmentCard crossfiresRifle
      , SomeAttachmentCard crossbonesMachineGun
      , SomeAttachmentCard crossbonesArmor
      , SomeAttachmentCard laserRifle
      , SomeAttachmentCard energyShield
      , SomeAttachmentCard powerGauntlets
      , SomeAttachmentCard exoSuit
      , SomeAttachmentCard absorbingMansBallAndChain
      , SomeAttachmentCard combatKnife
      , SomeAttachmentCard hydraSidearm

      -- Zola
      , SomeAttachmentCard defensiveProgramming
      , SomeAttachmentCard painInhibitors
      , SomeAttachmentCard neurologicalImplants

      -- Red Skull
      , SomeAttachmentCard redSkullsLuger
      , SomeAttachmentCard redSkullsRightHook
      , SomeAttachmentCard masterStrategist
      , SomeAttachmentCard twistedReality

      -- Taskmaster
      , SomeAttachmentCard taskmastersSword
      , SomeAttachmentCard taskmastersShield
      , SomeAttachmentCard photographicReflexes

      -- The Once and Future Kang
      , SomeAttachmentCard temporalShield
      , SomeAttachmentCard futureWeapon
      , SomeAttachmentCard frozenInTime


      -- Ant-Man
      , SomeAttachmentCard sizeIncrease

      -- Wasp
      , SomeAttachmentCard beetleArmorMkIV

      -- Quicksilver
      , SomeAttachmentCard vibrationResistance

      -- Scarlet Witch
      , SomeAttachmentCard magicalSuspension

      -- Gamora
      , SomeAttachmentCard inABind

      -- Rocket Raccoon
      , SomeAttachmentCard blackjacksBazooka

      -- The Galaxy's Most Wanted
      , SomeAttachmentCard powerStone
      -- Brotherhood of Badoon
      , SomeAttachmentCard drangsSpear
      -- Ronan the Accuser
      , SomeAttachmentCard universalWeapon
      , SomeAttachmentCard fanaticism
      , SomeAttachmentCard kreeCombatArmor
      -- The Galaxy's Most Wanted: Nebula
      , SomeAttachmentCard cutthroatAmbition
      , SomeAttachmentCard evasiveManeuvering
      , SomeAttachmentCard unyieldingPersistence
      , SomeAttachmentCard weaponMastery
      , SomeAttachmentCard wideStance

      -- The Galaxy's Most Wanted: Infiltrate the Museum, Galactic Artifacts
      , SomeAttachmentCard biogramImage
      , SomeAttachmentCard cloakOfHercules
      , SomeAttachmentCard obediencePotion
      , SomeAttachmentCard theBeyondersBlazer
      , SomeAttachmentCard thePoison
      , SomeAttachmentCard vandarianPowerWand
      -- Drax
      , SomeAttachmentCard challengeAccepted
      -- Spectrum
      , SomeAttachmentCard sapPower
      -- The Mad Titan's Shadow: Tower Defense
      , SomeAttachmentCard focusedDefense
      , SomeAttachmentCard proximasSpear
      , SomeAttachmentCard corvussGlaive
      , SomeAttachmentCard directAssault
      -- The Mad Titan's Shadow: Ebony Maw
      , SomeAttachmentCard abjuration
      , SomeAttachmentCard restrained
      -- The Mad Titan's Shadow: Thanos, Infinity Gauntlet
      , SomeAttachmentCard thanossArmor
      , SomeAttachmentCard thanossHelmet
      , SomeAttachmentCard masterOfTheStones
      , SomeAttachmentCard infinityGauntlet
      -- The Mad Titan's Shadow: Loki
      , SomeAttachmentCard lokisStaff
      , SomeAttachmentCard lokisCrown
      , SomeAttachmentCard lokisCape
      , SomeAttachmentCard masterOfIllusions
      -- The Mad Titan's Shadow: Hela, Legions of Hel, Frost Giants, Enchantress
      , SomeAttachmentCard nightsword
      , SomeAttachmentCard helasCrown
      , SomeAttachmentCard helasCloak
      , SomeAttachmentCard fallenWarrior
      , SomeAttachmentCard frozen
      , SomeAttachmentCard beguiled
      , SomeAttachmentCard seduced
      -- The Mad Titan's Shadow: the campaign set
      , SomeAttachmentCard jormungand
      , SomeAttachmentCard lethalWeapon
      , SomeAttachmentCard (reprintAs Cards.beguiledValkyrie beguiled)
      , SomeAttachmentCard (reprintAs Cards.seducedValkyrie seduced)
      ]

lookupAttachment :: CardCode -> AttachmentId -> Attachment
lookupAttachment cardCode = case lookup cardCode allAttachments of
  Just (SomeAttachmentCard a) -> Attachment <$> buildCard a
  Nothing -> error $ "Invalid card code for attachment " <> show cardCode
