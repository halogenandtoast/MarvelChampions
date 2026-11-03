module Marvel.CivilWar.DeadlyDuo where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.HellsKitchen qualified as Shared
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types

newtype DeadlyMinion = DeadlyMinion (Attrs Minion `With` Maybe IdentityId)
  deriving anyclass (HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

instance IsMinion DeadlyMinion where
  toMinionAttrs (DeadlyMinion (a `With` _)) = a
  overMinionAttrs f (DeadlyMinion (a `With` you)) = DeadlyMinion (f a `With` you)

deadlyMinions :: [MinionCard DeadlyMinion]
deadlyMinions = map make [C.cw57027,C.cw57028]
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
    in minion (DeadlyMinion . (`With` Nothing)) def (Sch sch) (Atk atk) (HP hp)

instance RunMessage DeadlyMinion where
  runMessage msg e@(DeadlyMinion (a `With` actor)) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      discardFromDeck you 3 a
      pure $ DeadlyMinion (a `With` Just you)
    WithDiscarded (isTarget a -> True) FromDeck cards -> do
      let n = fromIntegral $ length $ nub $ map (cdCardType . getCardDef) cards
      for_ actor $ \you -> when (n > 0) $
        if toCardCode a == "57027" then dealIndirectDamage you a n
        else do
          schemes <- asYou you $ selectList EncounterMainScheme
          traverse_ (\scheme -> placeThreatOn a scheme n) schemes
      pure e
    _ -> DeadlyMinion . (`With` actor) <$> runMessage msg a

-- Both printings have the same attachment text and boost effect.
madJacksPlatform :: AttachmentCard Shared.ResistanceFighter
madJacksPlatform = attachment Shared.ResistanceFighter C.cw57029

newtype JestersYoYo = JestersYoYo (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

jestersYoYo :: TreacheryCard JestersYoYo
jestersYoYo = treachery JestersYoYo C.cw57030

instance RunMessage JestersYoYo where
  runMessage msg e@(JestersYoYo a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      targets <- discardableControlledCardsMatching you AnyCard
      costed <- for targets $ \target -> do
        def <- getsGame (`abilityCardDef` target)
        pure (target,max 0 $ fromMaybe 0 $ cdCost =<< def)
      case costed of
        [] -> gainSurge a
        _ -> chooseAndDiscardControlled you [target | (target,cost) <- costed,cost == maximum (map snd costed)]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      discardableControlledCardsMatching you (CardWithType UpgradeType) >>= chooseAndDiscardControlled you
      pure e
    _ -> JestersYoYo <$> runMessage msg a

newtype DeadlyDuo = DeadlyDuo (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

deadlyDuo :: SideSchemeCard DeadlyDuo
deadlyDuo = sideScheme DeadlyDuo C.cw57031 (Static 3)

instance RunMessage DeadlyDuo where
  runMessage msg (DeadlyDuo a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      board <- getAreaOfTarget a
      players <- filterM (fmap (== board) . getAreaOfTarget) =<< getAllPlayers
      traverse_ (`discardTopOfDeck` 8) players
      DeadlyDuo <$> runMessage msg a
    _ -> DeadlyDuo <$> runMessage msg a
