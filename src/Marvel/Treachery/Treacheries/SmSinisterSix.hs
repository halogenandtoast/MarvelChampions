module Marvel.Treachery.Treacheries.SmSinisterSix where

import Marvel.Prelude
import Marvel.Card
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Status
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Villain.Villains.SmSinisterSix
import Marvel.Window qualified as W

newtype SixTreacheryMeta = SixTreacheryMeta {painAttack :: Maybe (VillainId, IdentityId, Natural)}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype SmSinisterSixTreachery = SmSinisterSixTreachery (Attrs Treachery `With` SixTreacheryMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery SmSinisterSixTreachery where
  toTreacheryAttrs (SmSinisterSixTreachery (attrs `With` _)) = attrs
  treacheryModifiersFor _ (TreacheryRef tid) (SmSinisterSixTreachery (attrs `With` _))
    | tid == treacheryId attrs, toCardCode attrs `elem` ["27108", "27109", "27110", "27112"] = do
        difficulty <- getDifficulty
        pure [CannotBeCanceled | difficulty == Expert]
  treacheryModifiersFor _ _ _ = pure []

sixTreachery :: Attrs Treachery -> SmSinisterSixTreachery
sixTreachery = SmSinisterSixTreachery . (`With` SixTreacheryMeta Nothing)

smFrequentFlyers, smHighFashion, smRoboticEnhancements, smPartnershipOfPain, smSurprise :: TreacheryCard SmSinisterSixTreachery
smFrequentFlyers = treachery sixTreachery Cards.smFrequentFlyers
smHighFashion = treachery sixTreachery Cards.smHighFashion
smRoboticEnhancements = treachery sixTreachery Cards.smRoboticEnhancements
smPartnershipOfPain = treachery sixTreachery Cards.smPartnershipOfPain
smSurprise = treachery sixTreachery Cards.smSurprise

lightAtEnd :: (HasGame m) => m [SideSchemeId]
lightAtEnd = (<>) <$> selectList (SideSchemeIs SideSchemes.smLightAtTheEndA) <*> selectList (SideSchemeIs SideSchemes.smLightAtTheEndB)

-- | Highest/lowest resource cost among cards controlled, evaluated when each
-- discard resolves, so High Fashion's second discard sees what the first removed.
discardControlledByCost :: (ReverseQueue m) => Bool -> IdentityId -> m ()
discardControlledByCost highest you = do
  allies <- selectList (AllyControlledBy $ IdentityWithId you)
  supports <- selectList (SupportControlledBy $ IdentityWithId you)
  upgrades <- selectList (UpgradeControlledBy $ IdentityWithId you)
  let candidates = map AllyRef allies <> map SupportRef supports <> map UpgradeRef upgrades
  legal <- filterM (fmap (not . any (`elem` [CannotLeavePlay, CannotBeDiscarded])) . getModifiers) candidates
  scored <- fmap catMaybes $ for legal \target -> do
    def <- getsGame (`abilityCardDef` target)
    pure $ (target,) <$> (cdCost =<< def)
  unless (null scored) do
    let extreme = (if highest then maximum else minimum) (map snd scored)
    chooseOne you [targeting target (removeFromPlay target) | (target, cost) <- scored, cost == extreme]

instance RunMessage SmSinisterSixTreachery where
  runMessage msg e@(SmSinisterSixTreachery (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage tid TreacheryIncite | tid == treacheryId attrs, toCardCode attrs `elem` ["27108", "27109", "27110"] -> do
      difficulty <- getDifficulty
      when (difficulty == Expert) $ selectList EncounterMainScheme >>= traverse_ (`placeThreat` 1)
      SmSinisterSixTreachery . (`With` meta) <$> runMessage msg attrs
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      present <- map snd <$> sixVillains
      let bring code penalty =
            if code `elem` present then cardSpecific attrs penalty you else cardSpecific GameRef "sinisterSixPutVillain" (code :: CardCode)
      case toCardCode attrs of
        "27108" -> bring "27096" "hobgoblinPresent" >> bring "27099" "vulturePresent" >> pure e
        "27109" -> bring "27095" "electroPresent" >> bring "27097" "kravenPresent" >> pure e
        "27110" -> bring "27094" "octopusPresent" >> bring "27098" "scorpionPresent" >> pure e
        "27111" -> partnership you
        _ -> do
          difficulty <- getDifficulty
          when (difficulty == Expert) $ gainSurge attrs
          ambushThen attrs "surpriseEntered" you
          pure e
    CardSpecific (isTarget attrs -> True) "surpriseEntered" (fromJSON -> Success (entered, _ :: Value)) -> do
      when (isNothing (entered :: Maybe VillainId)) $ lightAtEnd >>= traverse_ (`placeThreat` 3)
      pure e
    CardSpecific (isTarget attrs -> True) "partnershipEntered" (fromJSON -> Success (entered, fromJSON -> Success you)) ->
      if isJust (entered :: Maybe VillainId) then partnership (you :: IdentityId) else pure e
    CardSpecific (isTarget attrs -> True) key (fromJSON -> Success you)
      | key `elem` ["hobgoblinPresent", "vulturePresent", "electroPresent", "kravenPresent", "octopusPresent", "scorpionPresent"] -> do
          stillInGame <- elem you <$> getPlayers
          when stillInGame $ case key of
            "hobgoblinPresent" -> dealIndirectDamage you attrs 2
            "vulturePresent" -> discardFrom you RandomFromHand 1
            "electroPresent" -> discardControlledByCost True you
            "kravenPresent" -> discardControlledByCost False you
            "octopusPresent" -> statusControlledCharacter you ConfusedStatus
            _ -> statusControlledCharacter you StunnedStatus
          pure e
    CheckWindows windows
      | Just (vid, you, bonus) <- painAttack meta
      , W.Window W.When (W.EnemyAttack (EnemyVillainId vid) you) `elem` windows -> do
          changeAttack vid (fromIntegral bonus)
          pure $ SmSinisterSixTreachery (attrs `With` SixTreacheryMeta Nothing)
    _ -> SmSinisterSixTreachery . (`With` meta) <$> runMessage msg attrs
   where
    partnership you = do
      present <- sixVillains
      hero <- identityMatches HeroIdentity you
      if null present
        then e <$ ambushThen attrs "partnershipEntered" you
        else do
          let chosen = if hero then last present else head present
              vid = fst chosen
              others = filter ((/= vid) . fst) present
          if hero
            then do
              bonus <- sum <$> traverse (getVillainAttackValue . fst) others
              enemyAttacks vid you
              pure $ SmSinisterSixTreachery (attrs `With` SixTreacheryMeta (Just (vid, you, bonus)))
            else do
              bonus <- sum <$> traverse (getVillainSchemeValue . fst) others
              activationBonus vid bonus
              villainSchemesAgainst vid you
              pure e
