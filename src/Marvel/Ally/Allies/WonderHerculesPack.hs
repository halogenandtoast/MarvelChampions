module Marvel.Ally.Allies.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Attack
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Runner ()
import Marvel.Ally.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC

wm58012 :: AllyCard WmHcAlly
wm58012 = ally (WmHcAlly . (`With` False)) WM.wm58012 (Thw 2, 1) (Atk 2, 1) (HP 2)

wm58013 :: AllyCard WmHcAlly
wm58013 = allyWith (WmHcAlly . (`With` False)) WM.wm58013 (Thw 1, 1) (Atk 1, 1) (HP 3) (\a -> a {allyCounters = 4})

wm58014 :: AllyCard WmHcAlly
wm58014 = ally (WmHcAlly . (`With` False)) WM.wm58014 (Thw 2, 1) (Atk 2, 1) (HP 3)

wm58015 :: AllyCard WmHcAlly
wm58015 = ally (WmHcAlly . (`With` False)) WM.wm58015 (Thw 2, 1) (Atk 3, 1) (HP 5)

wm58022 :: AllyCard WmHcAlly
wm58022 = ally (WmHcAlly . (`With` False)) WM.wm58022 (Thw 2, 1) (Atk 2, 1) (HP 4)

hc59008 :: AllyCard WmHcAlly
hc59008 = ally (WmHcAlly . (`With` False)) HC.hc59008 (Thw 1, 1) (Atk 1, 1) (HP 2)

hc59018 :: AllyCard WmHcAlly
hc59018 = ally (WmHcAlly . (`With` False)) HC.hc59018 (Thw 1, 1) (Atk 3, 2) (HP 4)

hc59019 :: AllyCard WmHcAlly
hc59019 = ally (WmHcAlly . (`With` False)) HC.hc59019 (Thw 2, 1) (Atk 2, 1) (HP 4)

hc59020 :: AllyCard WmHcAlly
hc59020 = ally (WmHcAlly . (`With` False)) HC.hc59020 (Thw 3, 1) (Atk 3, 2) (HP 4)

hc59025 :: AllyCard WmHcAlly
hc59025 = ally (WmHcAlly . (`With` False)) HC.hc59025 (Thw 2, 1) (Atk 3, 1) (HP 6)

newtype WmHcAlly = WmHcAlly (Attrs Ally `With` Bool)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAlly WmHcAlly where
  toAllyAttrs (WmHcAlly (attrs `With` _)) = attrs

instance HasModifiersFor WmHcAlly where
  getModifiersFor _ target (WmHcAlly (attrs `With` deathcry)) | isTarget attrs target = case toCardCode attrs of
    "58022" -> pure [KeywordModifier K.Piercing]
    "59018" -> pure [AttackConsequentialDamageReduction 1 | deathcry]
    "59019" -> do
      others <- selectList $ AllyControlledBy (IdentityWithId $ allyController attrs) <> NotAlly (AllyWithId $ allyId attrs)
      pure [HitPointModifier $ fromIntegral $ length others]
    _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities WmHcAlly where
  getAbilities a@(WmHcAlly (attrs `With` _)) = case toCardCode attrs of
    "58012" -> [limitedWindowAbility a 1 (W.AllyWouldBeDefeatedByConsequentialDamage $ AllyWithId aid) Interrupt (OwnsThis <> AllyExists (AllyWithId aid <> AllyWithAnyUses)) UseCost $ runAbility a 1]
    "58013" -> [limitedWindowAbility a 1 (W.AllyThwarted W.After AnyAlly AnyScheme) Response OwnsThis UseCost $ runAbility a 1]
    "58014" -> [limitedWindowAbility a 1 (W.PlayThis W.After) ForcedResponse OwnsThis NoCost $ runAbility a 1]
    "58015" -> [limitedWindowAbility a 1 W.EntersPlayThis ForcedResponse OwnsThis NoCost $ runAbility a 1]
    "58022" -> [limitedWindowAbility a 1 (W.BoostCardFlippedDuringUndefendedAttack W.After AnyEnemy) HeroResponse OwnsThis NoCost $ runAbility a 1]
    "59008" -> [ability a 1 Action OwnsThis ExhaustCost $ runAbility a 1
               ,limitedWindowAbility a 2 (W.EnemyAttacksPlayerOrAlly W.When MinionEnemy $ IdentityWithId $ allyController attrs) ForcedInterrupt OwnsThis NoCost $ runAbility a 2]
    "59018" -> [limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId aid) AnyEnemy) HeroInterrupt OwnsThis (DamageCost 1) $ runAbility a 1]
    "59020" -> [limitedWindowAbility a 1 (W.AllyThwarted W.After (AllyWithId aid) AnyScheme) ForcedResponse OwnsThis NoCost $ runAbility a 1]
    _ -> []
   where aid = allyId attrs

instance RunMessage WmHcAlly where
  runMessage msg a@(WmHcAlly (attrs `With` deathcry)) = runQueueT $ case msg of
    AllyMessage aid (AllyPaidWith payment) | aid == allyId attrs && toCardCode attrs == "58012" -> pure $ WmHcAlly (attrs {allyCounters = if null $ overpaidResources payment then 0 else 1} `With` deathcry)
    AllyMessage aid (AllyPaidWith _) | aid == allyId attrs && toCardCode attrs == "58013" -> pure $ WmHcAlly (attrs {allyCounters = 4} `With` deathcry)
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case toCardCode attrs of
        "58012" -> do
          cancelMatchingMessage $ \case AllyMessage aid (AllyDamaged _ damage) -> aid == allyId attrs && damageSource damage == FromConsequential; _ -> False
          allyStep (allyId attrs) $ AllySetDamage 0
          pure a
        "58013" -> do
          chooseDamage you attrs (toDamage 2 FromAbility) MinionEnemy
          pure a
        "58014" -> do
          cards <- take 1 <$> getEncounterDeck
          for_ cards $ \card -> do
            discardCard card
            when (cardMatch (CardWithType TreacheryType) card) $ resolveEncounterWhenRevealed you card
          pure a
        "58015" -> do
          cards <- filter (cardMatch $ CardWithType SideSchemeType) <$> getEncounterDeck
          if null cards then placeThreatOnEach attrs 6 MainScheme
          else chooseOne you [targeting card $ revealEncounterCard you card | card <- cards]
          shuffleEncounterDeck
          pure a
        "58022" -> do
          for_ [enemy | W.FlippedBoostCard _ enemy <- windows] $ \enemy -> do
            attack <- getEnemyAttack enemy
            for_ attack $ \at -> unless (attackDefended at) do
              enemyDefendedBy enemy $ AllyCharacter $ allyId attrs
              checkWindows [W.Window W.When $ W.AllyDefendedAgainst (allyId attrs) enemy]
          pure a
        "59008" -> a <$ drawCards you 1
        "59018" -> pure $ WmHcAlly (attrs `With` True)
        "59020" -> a <$ discardUntilIntoPlay you (CardWithType MinionType)
        _ -> pure a
    RanAbility you (isTarget attrs -> True) 2 windows _ | toCardCode attrs == "59008" -> do
      for_ [enemy | W.EnemyAttack enemy who <- windows, who == you] $ \enemy -> redirectEnemyAttackToAlly enemy you $ allyId attrs
      pure a
    AllyMessage aid step | aid == allyId attrs && toCardCode attrs == "59018", (case step of AllyAttackConsequential -> True; AllyAttackConsequentialAgainst _ -> True; _ -> False) -> do
      attrs' <- runMessage msg attrs
      cardSpecific attrs "clearDeathcry" ()
      pure $ WmHcAlly (attrs' `With` deathcry)
    CardSpecific (isTarget attrs -> True) "clearDeathcry" _ -> pure $ WmHcAlly (attrs `With` False)
    _ -> (\attrs' -> WmHcAlly (attrs' `With` deathcry)) <$> runMessage msg attrs
