module Marvel.Villain.Villains.SmSinisterSix where

import Marvel.Prelude
import Data.List (sortOn)
import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Status
import Marvel.Window
import Marvel.Window qualified as W
import Marvel.Card.Def (cdCardCode)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types

newtype SmSinisterSixVillain = SmSinisterSixVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

smDoctorOctopus1 :: VillainCard SmSinisterSixVillain
smDoctorOctopus1 = villainWith SmSinisterSixVillain Cards.smDoctorOctopus1 (Sch 2) (Atk 2) (HP $ Static 8) (stageL .~ 1)

smElectro1 :: VillainCard SmSinisterSixVillain
smElectro1 = villainWith SmSinisterSixVillain Cards.smElectro1 (Sch 2) (Atk 1) (HP $ Static 8) (stageL .~ 1)

smHobgoblin1 :: VillainCard SmSinisterSixVillain
smHobgoblin1 = villainWith SmSinisterSixVillain Cards.smHobgoblin1 (Sch 2) (Atk 1) (HP $ Static 9) (stageL .~ 1)

smKravenTheHunter1 :: VillainCard SmSinisterSixVillain
smKravenTheHunter1 = villainWith SmSinisterSixVillain Cards.smKravenTheHunter1 (Sch 1) (Atk 2) (HP $ Static 9) (stageL .~ 1)

smScorpion1 :: VillainCard SmSinisterSixVillain
smScorpion1 = villainWith SmSinisterSixVillain Cards.smScorpion1 (Sch 0) (Atk 3) (HP $ Static 10) (stageL .~ 1)

smVulture1 :: VillainCard SmSinisterSixVillain
smVulture1 = villainWith SmSinisterSixVillain Cards.smVulture1 (Sch 1) (Atk 2) (HP $ Static 7) (stageL .~ 1)


-- | Card codes follow activation order, independent of the order they entered play.
sixCodes :: [CardCode]
sixCodes = ["27094", "27095", "27096", "27097", "27098", "27099"]

activationOrder :: CardCode -> Int
activationOrder code = fromMaybe 0 $ headMay [n | (candidate, n) <- zip sixCodes [1 ..], candidate == code]

sixVillains :: (HasGame m) => m [(VillainId, CardCode)]
sixVillains = do
  villains <- selectList AnyVillain
  found <- for villains \vid -> do
    def <- getsGame (`abilityCardDef` VillainRef vid)
    pure [(vid, code) | code <- maybe [] (pure . cdCardCode) def, code `elem` sixCodes]
  pure $ sortOn (activationOrder . snd) $ concat found

-- | The scenario owns the set-aside villains and all random selection. A callback
-- sees the game after the chosen villain has entered play and received the counter.
ambush :: (ReverseQueue m) => m ()
ambush = cardSpecific GameRef "sinisterSixAmbush" (Nothing :: Maybe (Target, Text, Value))

ambushThen :: (ReverseQueue m, IsRef target, ToJSON value) => target -> Text -> value -> m ()
ambushThen target key value = cardSpecific GameRef "sinisterSixAmbush" (Just (toTarget target, key, toJSON value))

nextActive :: (ReverseQueue m) => VillainId -> m ()
nextActive = cardSpecific GameRef "sinisterSixNextActive"

-- | Discarding must respect permanent cards and other discard restrictions.
discardSupportOrUpgrade :: (ReverseQueue m) => IdentityId -> m ()
discardSupportOrUpgrade you = do
  supports <- selectList (SupportControlledBy $ IdentityWithId you)
  upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
  legal <- filterM (fmap (not . any (`elem` [CannotLeavePlay, CannotBeDiscarded])) . getModifiers) (map SupportRef supports <> map UpgradeRef upgrades)
  unless (null legal) $ chooseOne you [targeting target (removeFromPlay target) | target <- legal]

statusControlledCharacter :: (ReverseQueue m) => IdentityId -> StatusCard -> m ()
statusControlledCharacter you status = do
  characters <- selectList (CharacterControlledBy $ IdentityWithId you)
  unless (null characters) $ chooseOne you [targeting (characterRef character) (giveStatus character status) | character <- characters]

instance HasAbilities SmSinisterSixVillain where
  getAbilities (SmSinisterSixVillain a) =
    [windowAbility a 1 (EnemyAttackedAndDamaged (EnemyWithId $ EnemyVillainId $ villainId a) $ IdentityCharacters AnyIdentity) ForcedResponse NoCost (runAbility a 1)]

instance RunMessage SmSinisterSixVillain where
  runMessage msg e@(SmSinisterSixVillain attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [you | W.EnemyAttacksAndDamages _ (IdentityCharacter you) _ <- windows] \you -> do
        case toCardCode (villainCardDef attrs) of
          "27094" -> selectList AnyScheme >>= traverse_ (`placeThreat` 1)
          "27095" -> discardTopOfDeck you 7
          "27096" -> dealIndirectDamage you attrs 2
          "27097" -> discardSupportOrUpgrade you
          "27098" -> statusControlledCharacter you StunnedStatus
          "27099" -> discardFromHand you 1
          _ -> pure ()
        nextActive (villainId attrs)
      pure e
    _ -> SmSinisterSixVillain <$> runMessage msg attrs
