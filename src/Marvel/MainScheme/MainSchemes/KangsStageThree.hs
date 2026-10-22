-- | The Once and Future Kang's four stage 3 schemes, each a game area of its own with its
-- Kang (II): The Chronopolis, Inexorable Fate, The Realm of Rama-Tut and The Present
-- Future War. Their text is the same but for the Kang each brings.
module Marvel.MainScheme.MainSchemes.KangsStageThree where

import Marvel.Prelude

import Marvel.Card
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Villain.Cards qualified as Villains

-- | Whether the stage has been completed, its Forced Response resolved.
newtype Meta = Meta {completed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype KangsStageThree = KangsStageThree (Attrs MainScheme `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasCardCode KangsStageThree where
  toCardCode (KangsStageThree (a `With` _)) = toCardCode a

instance IsMainScheme KangsStageThree where
  toMainSchemeAttrs (KangsStageThree (a `With` _)) = a

stageThree :: CardDef -> Natural -> MainSchemeCard KangsStageThree
stageThree def initial = mainScheme (KangsStageThree . (`With` Meta False)) def (Static 9) (Static $ fromIntegral initial) (Static 1)

theChronopolis :: MainSchemeCard KangsStageThree
theChronopolis = stageThree Cards.theChronopolis 0

inexorableFate :: MainSchemeCard KangsStageThree
inexorableFate = stageThree Cards.inexorableFate 1

theRealmOfRamaTut :: MainSchemeCard KangsStageThree
theRealmOfRamaTut = stageThree Cards.theRealmOfRamaTut 1

thePresentFutureWar :: MainSchemeCard KangsStageThree
thePresentFutureWar = stageThree Cards.thePresentFutureWar 2

-- | The stage's Kang (II), the standard and the Expert Kang set's.
stageKang :: CardCode -> (CardDef, CardDef)
stageKang = \case
  "11009" -> (Villains.kangImmortus, Villains.kangImmortusExpert)
  "11010" -> (Villains.kangIronLad, Villains.kangIronLadExpert)
  "11011" -> (Villains.kangRamaTut, Villains.kangRamaTutExpert)
  _ -> (Villains.kangScarletCenturion, Villains.kangScarletCenturionExpert)

-- | 3A When Revealed: create your own game area and place this scheme in it (as stage 2A
-- makes it); add the Kang (II) to the game area and deal yourself an encounter card.
--
-- 3B Forced Response: after this stage is completed, place 1 set-aside Kang's Dominion
-- facedown under stage 4A. At the end of the phase, remove the Kang (II) and this stage
-- from the game and combine your game area with another game area. If all the players
-- at this stage are defeated, this stage is completed.
instance RunMessage KangsStageThree where
  runMessage msg ms@(KangsStageThree (attrs `With` meta)) = runQueueT $ case msg of
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> do
      expert <- (== Expert) <$> getDifficulty
      let (standard, expertKang) = stageKang (toCardCode attrs)
      addVillain (toCardCode $ if expert then expertKang else standard)
      dealEncounterCard =<< getActivePlayerId
      pure ms
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) | not (completed meta) -> do
      cardSpecific GameRef "kangDominionUnderStage4" ()
      aid <- getAreaOfTarget (mainSchemeId attrs)
      let (standard, expertKang) = stageKang (toCardCode attrs)
      kangs <- filterM (fmap (`elem` [Just (toCardCode standard), Just (toCardCode expertKang)]) . villainCode) =<< getVillainsInArea aid
      atEndOfPhase do
        for_ kangs (removeFromGame . VillainRef)
        removeFromGame (MainSchemeRef $ mainSchemeId attrs)
        cardSpecific GameRef "kangJoinAnotherArea" aid
      pure . KangsStageThree $ attrs `With` Meta True
    AreaDefeated aid -> do
      here <- getAreaOfTarget (mainSchemeId attrs)
      when (aid == here) $ advanceMainScheme (mainSchemeId attrs)
      pure ms
    _ -> KangsStageThree . (`With` meta) <$> runMessage msg attrs
   where
    villainCode vid = getsGame (fmap toCardCode . (`abilityCardDef` VillainRef vid))
