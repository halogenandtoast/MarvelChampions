module Marvel.Treachery.Treacheries.NextEvolutionNeJuggernaut where

import Marvel.Ally.Types ()
import Marvel.Prelude
import Marvel.Status
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability ( HasAbilities )
import Marvel.Attack ()
import Marvel.Cost ()
import Marvel.Count ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
    ( Message(RevealedAsBoost, TreacheryMessage, CardSpecific, Boost),
      RunMessage(..),
      TreacheryMessage(RevealTreachery),
      runQueueT,
      cardSpecific,
      chooseOne,
      chooseOrRunOne,
      damageCharacter,
      damageIdentity,
      dealIndirectDamage,
      dealIndirectDamageAmongPlayers,
      enemyAttacksAlly,
      giveStatus,
      labeled,
      placeCounters,
      placeThreatOnEach,
      removeFromPlay,
      targeting )
import Marvel.Modifier
    ( HasModifiersFor(getModifiersFor), Modifier(KeywordModifier) )
import Marvel.NextEvolution.Juggernaut
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Window qualified as W ()
import Marvel.Card
import Marvel.Entity
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types


import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()


newtype NeJuggernautTreachery = NeJuggernautTreachery (Attrs Treachery)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40125 :: TreacheryCard NeJuggernautTreachery
ne40125 = treachery NeJuggernautTreachery Cards.ne40125

ne40126 :: TreacheryCard NeJuggernautTreachery
ne40126 = treachery NeJuggernautTreachery Cards.ne40126

ne40127 :: TreacheryCard NeJuggernautTreachery
ne40127 = treachery NeJuggernautTreachery Cards.ne40127

ne40128 :: TreacheryCard NeJuggernautTreachery
ne40128 = treachery NeJuggernautTreachery Cards.ne40128

ne40129 :: TreacheryCard NeJuggernautTreachery
ne40129 = treachery NeJuggernautTreachery Cards.ne40129


instance IsTreachery NeJuggernautTreachery where
  toTreacheryAttrs (NeJuggernautTreachery attrs) = attrs
  treacheryModifiersFor = getModifiersFor

instance HasModifiersFor NeJuggernautTreachery where
  getModifiersFor _ (TreacheryRef tid) (NeJuggernautTreachery attrs) | tid == treacheryId attrs, toCardCode attrs == "40129" = do
    difficulty <- getDifficulty
    pure [KeywordModifier $ Incite 1 | difficulty == Expert]
  getModifiersFor _ _ _ = pure []

instance RunMessage NeJuggernautTreachery where
  runMessage msg a@(NeJuggernautTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      juggernaut >>= traverse_ \vid -> do
        attack <- getVillainAttackValue vid
        immune <- cannotTakeDamageFrom (IdentityRef you) (toSource attrs)
        case toCardCode attrs of
          "40125" -> do
            highest <- highestCostUpgradeOrSupport you
            chooseOne you $
              [labeled "$label.neTakeJuggernautDamage" (damageIdentity you attrs $ toDamage attack FromTreachery) | attack > 0 && not immune]
              <> [labeled "$label.neDiscardHighest" (cardSpecific attrs "discardHighest" you) | notNull highest]
          "40126" -> chooseOne you $
            [labeled "$label.neTakeJuggernautDamage" (damageIdentity you attrs $ toDamage attack FromTreachery) | attack > 0 && not immune]
            <> [labeled "$label.nePlaceMomentum" (placeCounters vid 1)]
          "40127" -> dealIndirectDamageAmongPlayers attrs attack
          "40128" -> do
            hero <- identityMatches HeroIdentity you
            if not hero then dealIndirectDamage you attrs 2 else do
              allies <- leastRemainingAllies
              unless (null allies) $ chooseOrRunOne you [targeting aid do
                owner <- getSourceController (AllyRef aid)
                for_ owner (\player -> enemyAttacksAlly vid player aid)
                | aid <- allies]
          "40129" -> do
            exposed <- juggernautExposed
            if exposed
              then restoreJuggernautHelmet >> placeCounters vid 1
              else placeThreatOnEach attrs attack EncounterMainScheme
          _ -> pure ()
      pure a
    CardSpecific (isTarget attrs -> True) "discardHighest" (fromJSON -> Success you) -> do
      highest <- highestCostUpgradeOrSupport you
      unless (null highest) $ chooseOrRunOne you [targeting target (removeFromPlay target) | target <- highest]
      pure a
    Boost (RevealedAsBoost target enemy) | target == toTarget attrs -> do
      case toCardCode attrs of
        "40126" -> juggernaut >>= traverse_ (\vid -> giveStatus vid ToughStatus)
        "40127" -> do
          you <- getBoostedPlayer enemy
          dealIndirectDamage you attrs 1
        "40128" -> do
          you <- getBoostedPlayer enemy
          allies <- selectList (AllyControlledBy $ IdentityWithId you)
          valid <- filterM (fmap not . flip cannotTakeDamageFrom (toSource attrs) . toTarget) allies
          unless (null valid) $ chooseOrRunOne you [targeting aid (damageCharacter aid attrs $ toDamage 1 FromAbility) | aid <- valid]
        _ -> pure ()
      pure a
    _ -> NeJuggernautTreachery <$> runMessage msg attrs
