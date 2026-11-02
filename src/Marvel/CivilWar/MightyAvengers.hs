module Marvel.CivilWar.MightyAvengers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Card.WhenRevealed (hasWhenRevealed)
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Damage
import Marvel.Entity
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Game.Source
import Marvel.Hp
import Marvel.GameValue
import Marvel.Query
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types
import Marvel.Message.Lifted
import Marvel.Minion.Runner

newtype MightyAvenger = MightyAvenger (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

definitions :: [CardDef]
definitions = [C.cw56073,C.cw56074,C.cw56075,C.cw56078,C.cw56079]

minions :: [MinionCard MightyAvenger]
minions = map make definitions
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
             in minion MightyAvenger def (Sch sch) (Atk atk) (HP hp)

instance RunMessage MightyAvenger where
  runMessage msg e@(MightyAvenger a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> e <$ revealed you
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ (getBoostedPlayer enemy >>= boosted)
    _ -> MightyAvenger <$> runMessage msg a
   where
    revealed you = case toCardCode a of
      "56073" -> enemyLeaderFor you >>= traverse_ enemyBecomesTough
      "56074" -> placeThreatOnEach a 1 EncounterMainScheme
      "56075" -> damageIdentity you a (toDamage 1 FromAbility)
      "56078" -> do
        already <- identityMatches ConfusedIdentity you
        if already then placeThreatOnEach a 2 EncounterMainScheme else confuseIdentity you
      "56079" -> do
        already <- identityMatches StunnedIdentity you
        if already then dealIndirectDamage you a 2 else stunIdentity you
      _ -> pure ()
    boosted you = case toCardCode a of
      "56078" -> confuseIdentity you
      "56079" -> stunIdentity you
      _ -> revealed you

newtype SharedAvengerKeywords = SharedAvengerKeywords (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

earthsMightiestHeroes :: SideSchemeCard SharedAvengerKeywords
earthsMightiestHeroes = sideScheme SharedAvengerKeywords C.cw56077 (Static 3)

instance HasModifiersFor SharedAvengerKeywords where
  getModifiersFor _ (MinionRef mid) _ = do
    avenger <- minionMatches (MinionWithTrait T.Avenger) mid
    if not avenger then pure [] else do
      others <- selectList (MinionWithTrait T.Avenger <> NotMinion (MinionWithId mid))
      printed <- for others $ \other -> getsGame $ maybe [] (toList . cdKeywords) . (`abilityCardDef` MinionRef other)
      pure $ map KeywordModifier (concat printed)
  getModifiersFor _ _ _ = pure []

instance RunMessage SharedAvengerKeywords where
  runMessage msg (SharedAvengerKeywords a) = SharedAvengerKeywords <$> runMessage msg a

newtype MightyAvengersTreachery = MightyAvengersTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

treacheries :: [TreacheryCard MightyAvengersTreachery]
treacheries = map (treachery MightyAvengersTreachery) [C.cw56076,C.cw56081]

instance RunMessage MightyAvengersTreachery where
  runMessage msg e@(MightyAvengersTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      candidates <- selectList AnyMinion
      revealed <- filterM (\mid -> do
        blanked <- isBlanked mid
        printed <- getsGame $ maybe False (hasWhenRevealed . toCardCode) . (`abilityCardDef` MinionRef mid)
        keywords <- getEnemyKeywords (EnemyMinionId mid)
        pure (not blanked && (printed || K.Surge `elem` keywords))) candidates
      if null revealed then gainSurge a else cardSpecific a "resolveMinions" (you,revealed)
      pure e
    CardSpecific (isTarget a -> True) "resolveMinions" (fromJSON -> Success (you,remaining :: [MinionId])) -> do
      present <- filterM (minionMatches AnyMinion) remaining
      unless (null present) $ chooseOrRunOne you
        [targeting (MinionRef mid) $ do
          minionStep mid (RevealMinion you)
          keywords <- getEnemyKeywords (EnemyMinionId mid)
          traverse_ (const $ dealEncounterCard you) (filter (== K.Surge) keywords)
          cardSpecific a "resolveMinions" (you,filter (/= mid) present)
        | mid <- present]
      pure e
    _ -> MightyAvengersTreachery <$> runMessage msg a
