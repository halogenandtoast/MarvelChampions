module Marvel.CivilWar.ResistanceAvengers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Window qualified as W

newtype ResistanceAvenger = ResistanceAvenger (Attrs Minion `With` Bool)
  deriving anyclass (HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

instance IsMinion ResistanceAvenger where
  toMinionAttrs (ResistanceAvenger (a `With` _)) = a
  overMinionAttrs f (ResistanceAvenger (a `With` extra)) = ResistanceAvenger (f a `With` extra)

resistanceAvengers :: [MinionCard ResistanceAvenger]
resistanceAvengers = map make [C.cw56150,C.cw56151,C.cw56152,C.cw56155,C.cw56156]
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
             in minion (\a -> ResistanceAvenger (a `With` False)) def (Sch sch) (Atk atk) (HP hp)

instance RunMessage ResistanceAvenger where
  runMessage msg e@(ResistanceAvenger (a `With` extra)) = runQueueT $ case msg of
    MinionMessage mid MinionDefeated | mid == minionId a -> do
      blanked <- isBlanked a
      unless blanked $ enemyLeaderFor (minionEngagedIdentity a) >>= traverse_ (giveDefeatedMinionAsBoost mid)
      a' <- runMessage msg a
      pure $ ResistanceAvenger (a' `With` extra)
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      already <- case toCardCode a of
        "56150" -> do
          before <- identityMatches ConfusedIdentity you
          confuseIdentity you
          pure before
        "56151" -> do
          before <- identityMatches StunnedIdentity you
          stunIdentity you
          pure before
        "56152" -> do
          leader <- enemyLeaderFor you
          before <- maybe (pure 0) (\vid -> getCharacterStatusCount (VillainCharacter vid) ToughStatus) leader
          traverse_ (`giveStatus` ToughStatus) leader
          pure $ before > 0
        "56155" -> do
          upgrades <- asYou you $ selectList $ UpgradeControlledBy (IdentityWithId you) <> UnexhaustedUpgrade
          traverse_ exhaust upgrades
          pure False
        "56156" -> do
          identities <- asYou you $ selectList $ IdentityWithId you <> UnexhaustedIdentity
          allies <- asYou you $ selectList $ AllyControlledBy (IdentityWithId you) <> UnexhaustedAlly
          let choices = [targeting (characterRef $ IdentityCharacter iid) (exhaust iid) | iid <- identities]
                <> [targeting (characterRef $ AllyCharacter aid) (exhaust aid) | aid <- allies]
          unless (null choices) $ chooseOne you choices
          pure False
        _ -> pure False
      pure $ ResistanceAvenger (a `With` already)
    Boost (CheckWindows windows) | extra -> do
      for_ [card | W.Window W.When (W.CountingBoostIcons card) <- windows,ecCardId card == CardId (unMinionId $ minionId a)] $ \card -> changeBoostIconCount card (+ 2)
      pure e
    _ -> (\a' -> ResistanceAvenger (a' `With` extra)) <$> runMessage msg a

newtype AvengersSideScheme = AvengersSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

resistanceSideSchemes :: [SideSchemeCard AvengersSideScheme]
resistanceSideSchemes = map (\def -> sideScheme AvengersSideScheme def (Static 3)) [C.cw56154,C.cw56159]

instance RunMessage AvengersSideScheme where
  runMessage msg (AvengersSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      blanked <- isBlanked a
      unless blanked $ do
        area <- getAreaOfTarget a
        leader <- getActiveVillainIn area
        you <- maybe (getDefeatedBy a) firstPlayerAtLeader leader
        key <- getDeckKeyFor you
        if toCardCode a == "56154" then do
          cards <- filter (cardMatch $ CardWithType MinionType) <$> asYou you getEncounterDiscard
          unless (null cards) $ fromDeck key $ shuffleIntoEncounterDeck cards
        else do
          cards <- take 5 . pileDeck <$> getPile key
          for_ leader $ \vid -> lookForEnemyTeam you a cards
            [cardLabeled (toCardCode card) (giveCardAsBoost card vid) | card <- cards]
          fromDeck key shuffleEncounterDeck
      AvengersSideScheme <$> runMessage msg a
    _ -> AvengersSideScheme <$> runMessage msg a
