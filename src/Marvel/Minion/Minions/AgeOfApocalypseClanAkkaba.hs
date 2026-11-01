module Marvel.Minion.Minions.AgeOfApocalypseClanAkkaba where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Scenario.AgeOfApocalypseEnSabahNur
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaClanAkkabaMinion = AoaClanAkkabaMinion (Attrs Minion `With` ClanMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ozymandias :: MinionCard AoaClanAkkabaMinion
ozymandias = minion (AoaClanAkkabaMinion . (`With` ClanMeta False False)) Cards.ozymandias (Sch 1) (Atk 1) (HP 6)

scarab :: MinionCard AoaClanAkkabaMinion
scarab = minion (AoaClanAkkabaMinion . (`With` ClanMeta False False)) Cards.scarab (Sch 1) (Atk 3) (HP 5)

clanAkkabaZealot :: MinionCard AoaClanAkkabaMinion
clanAkkabaZealot = minion (AoaClanAkkabaMinion . (`With` ClanMeta False False)) Cards.clanAkkabaZealot (Sch 2) (Atk 2) (HP 3)

data ClanMeta = ClanMeta {killedAlly :: Bool, extraIcons :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsMinion AoaClanAkkabaMinion where
  toMinionAttrs (AoaClanAkkabaMinion (attrs `With` _)) = attrs
  overMinionAttrs f (AoaClanAkkabaMinion (attrs `With` meta)) = AoaClanAkkabaMinion (f attrs `With` meta)

instance HasCardCode AoaClanAkkabaMinion where
  toCardCode (AoaClanAkkabaMinion (attrs `With` _)) = toCardCode attrs

instance IsRef AoaClanAkkabaMinion where
  toRef (AoaClanAkkabaMinion (attrs `With` _)) = toRef attrs

instance HasModifiersFor AoaClanAkkabaMinion where
  getModifiersFor _ target (AoaClanAkkabaMinion (attrs `With` _)) | isTarget attrs target, toCardCode attrs == "45159" = do
    ritual <- ancientRitual
    pure $ maybe [] (pure . MinionSchemesOn) ritual
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaClanAkkabaMinion where
  getAbilities a@(AoaClanAkkabaMinion (attrs `With` _)) =
    [limitedWindowAbility a 1 (W.EnemyAttacked W.After (EnemyWithId $ EnemyMinionId $ minionId attrs) AnyIdentity)
      ForcedResponse NoCriteria NoCost (runAbility a 1) | toCardCode attrs == "45160"]
instance RunMessage AoaClanAkkabaMinion where
  runMessage msg a@(AoaClanAkkabaMinion (attrs `With` meta)) = runQueueT $ case msg of
    MinionMessage mid (MinionBeginAttack _) | mid == minionId attrs -> do
      attrs' <- runMessage msg attrs
      pure $ AoaClanAkkabaMinion $ attrs' `With` meta {killedAlly = False}
    CheckWindows windows | any (\case W.Window W.After (W.DefeatedAlly _ (Just (EnemyMinionId mid))) -> mid == minionId attrs; _ -> False) windows -> do
      attrs' <- runMessage msg attrs
      pure $ AoaClanAkkabaMinion $ attrs' `With` meta {killedAlly = True}
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ placeAncientRitualThreat (if killedAlly meta then 3 else 1)
    MinionMessage mid MinionDefeated | mid == minionId attrs, toCardCode attrs == "45161" -> do
      placeAncientRitualThreat 2
      AoaClanAkkabaMinion . (`With` meta) <$> runMessage msg attrs
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      case toCardCode attrs of
        "45159" -> do
          you <- getBoostedPlayer enemy
          chooseOne you
            [labeled (countLabel "aoaPlaceAncientRitualThreat" 3) $ placeAncientRitualThreat 3,
             labeled "$label.aoaThreeBoostIcons" $ cardSpecific attrs "extraBoostIcons" ()]
        "45161" -> placeAncientRitualThreat 1
        _ -> pure ()
      pure a
    Boost (CardSpecific target "extraBoostIcons" _) | isTarget attrs target -> pure $ AoaClanAkkabaMinion $ attrs `With` meta {extraIcons = True}
    Boost (CheckWindows windows) | extraIcons meta -> do
      for_ [card | W.Window _ (W.CountingBoostIcons card) <- windows, toCardCode card == toCardCode attrs] $ \card -> changeBoostIconCount card (+3)
      pure a
    _ -> AoaClanAkkabaMinion . (`With` meta) <$> runMessage msg attrs
