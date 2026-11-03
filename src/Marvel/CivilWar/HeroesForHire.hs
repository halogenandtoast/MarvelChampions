module Marvel.CivilWar.HeroesForHire where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

newtype HiredHero = HiredHero (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

hiredHeroes :: [MinionCard HiredHero]
hiredHeroes =
  [minion HiredHero C.cw56112 (Sch 1) (Atk 2) (HP 3)
  ,minion HiredHero C.cw56113 (Sch 2) (Atk 2) (HP 4)]

instance RunMessage HiredHero where
  runMessage msg e@(HiredHero a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      let kind = if toCardCode a == "56112" then AllyType else UpgradeType
      cards <- discardableControlledCardsMatching you (CardWithType kind)
      unregistered <- asYou you $ identityMatches (IdentityWithTrait T.Unregistered) you
      let choices = [targeting target $ discardControlled target | target <- cards]
      if unregistered then chooseForEnemyTeam you a choices else chooseOne you choices
      pure e
    _ -> HiredHero <$> runMessage msg a

newtype BountyHunting = BountyHunting (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

bountyHunting :: [TreacheryCard BountyHunting]
bountyHunting = map (treachery BountyHunting) [C.cw56115,C.cw56119]

instance RunMessage BountyHunting where
  runMessage msg e@(BountyHunting a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      unregistered <- asYou you $ identityMatches (IdentityWithTrait T.Unregistered) you
      discardFor you a (if unregistered then RandomFromHand else FromHand) 1 1
      pure $ BountyHunting a {treacheryResolver = Just you}
    WithDiscarded (isTarget a -> True) _ cards -> do
      let amount = fromIntegral $ sum $ map (length . printedResources . getCardDef) cards
      when (amount > 0) $ for_ (treacheryResolver a) $ \you -> do
        schemes <- asYou you $ selectList EncounterMainScheme
        for_ schemes $ \scheme -> placeThreatOn a scheme amount
      pure e
    _ -> BountyHunting <$> runMessage msg a

newtype HeroesForHire = HeroesForHire (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

heroesForHire :: SideSchemeCard HeroesForHire
heroesForHire = sideScheme HeroesForHire C.cw56116 (Static 3)

instance RunMessage HeroesForHire where
  runMessage msg (HeroesForHire a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      you <- getDefeatedBy a
      dealUnregisteredSuperForEnemyTeam you a
      HeroesForHire <$> runMessage msg a
    _ -> HeroesForHire <$> runMessage msg a
