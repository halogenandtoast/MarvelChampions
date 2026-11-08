module Marvel.Ally.Allies.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Name qualified as Name
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
  getModifiersFor _ target (WmHcAlly (attrs `With` deathcry)) | isTarget attrs target = case Name.title (cdName (allyCardDef attrs)) of
    "Swordsman" -> pure [KeywordModifier K.Piercing]
    "Deathcry" -> pure [AttackConsequentialDamageReduction 1 | deathcry]
    "Namora" -> do
      others <- selectList $ AllyControlledBy (IdentityWithId $ allyController attrs) <> NotAlly (AllyWithId $ allyId attrs)
      pure [HitPointModifier $ fromIntegral $ length others]
    _ -> pure []
  getModifiersFor _ _ _ = pure []

instance HasAbilities WmHcAlly where
  getAbilities a@(WmHcAlly (attrs `With` _)) = case Name.title (cdName (allyCardDef attrs)) of
    "Firebird" -> [limitedWindowAbility a 1 (W.AllyWouldBeDefeatedByConsequentialDamage $ AllyWithId aid) Interrupt (OwnsThis <> AllyExists (AllyWithId aid <> AllyWithAnyUses <> AllyCanBeHealed)) UseCost $ runAbility a 1]
    "Hawkeye" -> [limitedWindowAbility a 1 (W.AllyThwarted W.After AnyAlly AnyScheme) Response OwnsThis UseCost $ runAbility a 1]
    "Scarlet Witch" -> [limitedWindowAbility a 1 (W.PlayThis W.After) ForcedResponse OwnsThis NoCost $ runAbility a 1]
    "Sentry" -> [limitedWindowAbility a 1 W.EntersPlayThis ForcedResponse OwnsThis NoCost $ runAbility a 1]
    "Swordsman" -> [limitedWindowAbility a 1 (W.BoostCardFlippedDuringUndefendedAttack W.After AnyEnemy) HeroResponse OwnsThis NoCost $ runAbility a 1]
    "Amadeus Cho" -> [ability a 1 Action OwnsThis ExhaustCost $ runAbility a 1
               ,limitedWindowAbility a 2 (W.EnemyAttacksPlayerOrAlly W.When MinionEnemy $ IdentityWithId $ allyController attrs) ForcedInterrupt OwnsThis NoCost $ runAbility a 2]
    "Deathcry" -> [limitedWindowAbility a 1 (W.AllyAttacked W.When (AllyWithId aid) AnyEnemy) HeroInterrupt OwnsThis (DamageCost 1) $ runAbility a 1]
    "Thor" -> [limitedWindowAbility a 1 (W.AllyThwarted W.After (AllyWithId aid) AnyScheme) ForcedResponse OwnsThis NoCost $ runAbility a 1]
    _ -> []
   where aid = allyId attrs

instance RunMessage WmHcAlly where
  runMessage msg a@(WmHcAlly (attrs `With` deathcry)) = runQueueT $ case msg of
    AllyMessage aid (AllyPaidWith payment) | aid == allyId attrs && Name.title (cdName (allyCardDef attrs)) == "Firebird" -> pure $ WmHcAlly (attrs {allyCounters = if null $ overpaidResources payment then 0 else 1} `With` deathcry)
    AllyMessage aid (AllyPaidWith _) | aid == allyId attrs && Name.title (cdName (allyCardDef attrs)) == "Hawkeye" -> pure $ WmHcAlly (attrs {allyCounters = 4} `With` deathcry)
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case Name.title (cdName (allyCardDef attrs)) of
        "Firebird" -> do
          cancelMatchingMessage $ \case AllyMessage aid (AllyDamaged _ damage) -> aid == allyId attrs && damageSource damage == FromConsequential; _ -> False
          allyStep (allyId attrs) $ AllySetDamage 0
          pure a
        "Hawkeye" -> do
          chooseDamage you attrs (toDamage 2 FromAbility) MinionEnemy
          pure a
        "Scarlet Witch" -> do
          cards <- take 1 <$> getEncounterDeck
          for_ cards $ \card -> do
            discardCard card
            when (cardMatch (CardWithType TreacheryType) card) $ resolveEncounterWhenRevealed you card
          pure a
        "Sentry" -> do
          cards <- filter (cardMatch $ CardWithType SideSchemeType) <$> getEncounterDeck
          if null cards then placeThreatOnEach attrs 6 MainScheme
          else chooseOne you [targeting card $ revealEncounterCard you card | card <- cards]
          shuffleEncounterDeck
          pure a
        "Swordsman" -> do
          for_ [enemy | W.FlippedBoostCard _ enemy <- windows] $ \enemy -> do
            attack <- getEnemyAttack enemy
            for_ attack $ \at -> unless (attackDefended at) do
              enemyDefendedBy enemy $ AllyCharacter $ allyId attrs
              checkWindows [W.Window W.When $ W.AllyDefendedAgainst (allyId attrs) enemy]
          pure a
        "Amadeus Cho" -> a <$ drawCards you 1
        "Deathcry" -> pure $ WmHcAlly (attrs `With` True)
        "Thor" -> a <$ discardUntilIntoPlay you (CardWithType MinionType)
        _ -> pure a
    RanAbility you (isTarget attrs -> True) 2 windows _ | Name.title (cdName (allyCardDef attrs)) == "Amadeus Cho" -> do
      for_ [enemy | W.EnemyAttack enemy who <- windows, who == you] $ \enemy -> redirectEnemyAttackToAlly enemy you $ allyId attrs
      pure a
    AllyMessage aid step | aid == allyId attrs && Name.title (cdName (allyCardDef attrs)) == "Deathcry", (case step of AllyAttackConsequential -> True; AllyAttackConsequentialAgainst _ -> True; _ -> False) -> do
      attrs' <- runMessage msg attrs
      cardSpecific attrs "clearDeathcry" ()
      pure $ WmHcAlly (attrs' `With` deathcry)
    CardSpecific (isTarget attrs -> True) "clearDeathcry" _ -> pure $ WmHcAlly (attrs `With` False)
    _ -> (\attrs' -> WmHcAlly (attrs' `With` deathcry)) <$> runMessage msg attrs
