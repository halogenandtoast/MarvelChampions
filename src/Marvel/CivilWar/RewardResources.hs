module Marvel.CivilWar.RewardResources where

import Marvel.Prelude
import Marvel.Card
import Marvel.Count (selectCount)
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

-- | The spender, including a contributor or a resource spent from a holding card,
-- gets the response. Paying with these cards still generates two wild resources
-- when their Hero Response has no eligible friendly leader.
leaderRewardSpent :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
leaderRewardSpent you card = when (toCardCode card `elem` ["56132","56136","56210","56214","57035"]) do
  hero <- identityMatches HeroIdentity you
  when hero $ friendlyLeaderFor you >>= traverse_ \leader -> do
    family <- getsGame $ \g -> cdEncounterSet =<< abilityCardDef g (VillainRef leader)
    let named code = isJust family && family == cdEncounterSet (getCardDef $ civilWarDef code)
        offer label effect = chooseOne you [labeled label effect,labeled "$label.noResponse" (pure ())]
        countersOn def cap label = do
          attachments <- inAnyArea $ selectList $ AttachmentOnEnemy (EnemyVillainId leader) <> AttachmentIs def
          eligible <- filterM (\aid -> maybe (pure True) (\maximum -> (< maximum) <$> getCountersOn aid) cap) attachments
          unless (null eligible) $ offer label $ for_ eligible (`placeCounters` 2)
    case toCardCode card of
      "57035" -> do
        sources <- getMainSchemesIn =<< getAreaOfTarget you
        destinations <- getMainSchemesIn =<< getAreaOfTarget leader
        for_ [(SchemeMainSchemeId from,SchemeMainSchemeId to) | from <- sources,to <- destinations,from /= to] $ \(from,to) -> do
          amount <- selectCount SchemeThreat (SchemeWithId from)
          allowed <- canMoveSchemeThreatFrom (IdentityRef you) from
          when (amount > 0 && allowed) $ offer "$label.cwLegalCounsel" $ moveSchemeThreat (IdentityRef you) from to 1
      "56132" | named "56059" -> do
        tough <- getCharacterStatusCount (VillainCharacter leader) ToughStatus
        modifiers <- getModifiers leader
        let limit = 1 + sum [n | AdditionalToughStatusCards n <- modifiers]
        when (CanHaveAnyNumberOfToughStatusCards `elem` modifiers || tough < limit) $
          offer "$label.cwSuitUp" (giveStatus leader ToughStatus)
      "56136" | named "56092" -> countersOn C.cw56098 Nothing "$label.cwRawPower"
      "56210" | named "56137" -> do
        decks <- getDeckKeys
        let key = if VillainDeckKey leader `elem` decks then VillainDeckKey leader else EncounterDeckKey
        offer "$label.cwPeoplesHero" (fromDeck key $ dealBoost leader)
      "56214" | named "56168" -> countersOn C.cw56174 (Just 3) "$label.cwSecretContact"
      _ -> pure ()

-- | Interrupts to spending precede every after-spending response and the paid
-- card or ability. The actual spender owns the optional interrupt.
leaderRewardWhenSpent :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
leaderRewardWhenSpent you card = when (toCardCode card == "57077") do
  hero <- identityMatches HeroIdentity you
  when hero $ friendlyLeaderFor you >>= traverse_ \leader -> do
    family <- getsGame $ \g -> cdEncounterSet =<< abilityCardDef g (VillainRef leader)
    when (family == cdEncounterSet (getCardDef C.cw57040)) do
      forms <- inAnyArea $ concatMapM (\def -> selectList $ AttachmentOnEnemy (EnemyVillainId leader) <> AttachmentIs def) [C.cw57046a,C.cw57046b]
      unless (null forms) $ chooseOne you
        [labeled "$label.cwAndroidAssistance" (flipLeaderMassForm $ EnemyVillainId leader),labeled "$label.noResponse" (pure ())]
