module Marvel.Minion.Minions.AgeOfApocalypsePrelates where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Count
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDeck))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Window qualified as W
import Marvel.Card ()
import Marvel.Entity
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaPrelatesMinion = AoaPrelatesMinion (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

misterSinisterOverseer :: MinionCard AoaPrelatesMinion
misterSinisterOverseer = minion AoaPrelatesMinion Cards.misterSinisterOverseer (Sch 1) (Atk 1) (HP 5)

theShadowKingOverseer :: MinionCard AoaPrelatesMinion
theShadowKingOverseer = minion AoaPrelatesMinion Cards.theShadowKingOverseer (Sch 3) (Atk 1) (HP 5)

abyssOverseer :: MinionCard AoaPrelatesMinion
abyssOverseer = minion AoaPrelatesMinion Cards.abyssOverseer (Sch 2) (Atk 2) (HP 5)

sugarManOverseer :: MinionCard AoaPrelatesMinion
sugarManOverseer = minion AoaPrelatesMinion Cards.sugarManOverseer (Sch 1) (Atk 3) (HP 5)

mikhailRasputinOverseer :: MinionCard AoaPrelatesMinion
mikhailRasputinOverseer = minion AoaPrelatesMinion Cards.mikhailRasputinOverseer (Sch 2) (Atk 2) (HP 5)

instance HasModifiersFor AoaPrelatesMinion where
  getModifiersFor _ target (AoaPrelatesMinion attrs) | isTarget attrs target = do
    hp <- fromIntegral <$> fromGameValue (PerPlayer 5)
    pure $ [HitPointModifier $ hp - 5 + if toCardCode attrs == "45181b" then 2 * fromIntegral (length $ minionHeldCards attrs) else 0]
      <> [KeywordModifier Piercing | toCardCode attrs == "45182b"]
  getModifiersFor _ _ _ = pure []

instance HasAbilities AoaPrelatesMinion where
  getAbilities a@(AoaPrelatesMinion attrs) = case toCardCode attrs of
    "45180b" -> [afterAttack]
    "45181b" -> [limitedWindowAbility a 1 (W.EnemyActivates W.After own AnyIdentity) ForcedResponse NoCriteria NoCost (runAbility a 1)]
    "45182b" -> [limitedWindowAbility a 1 (W.CharacterDefeatedByEnemyAttack W.After own) ForcedResponse NoCriteria NoCost (runAbility a 1)]
    "45183b" -> [limitedWindowAbility a 1 (W.EnemyAttacked W.When own AnyIdentity) ForcedInterrupt NoCriteria NoCost (runAbility a 1)]
    _ -> []
   where
    own = EnemyWithId $ EnemyMinionId $ minionId attrs
    afterAttack = limitedWindowAbility a 1 (W.EnemyAttacked W.After own AnyIdentity) ForcedResponse NoCriteria NoCost (runAbility a 1)
instance RunMessage AoaPrelatesMinion where
  runMessage msg a@(AoaPrelatesMinion attrs) = runQueueT $ case msg of
    CheckWindows windows | any (\case W.Window W.After (W.MinionEnteredPlay mid) -> mid == minionId attrs; _ -> False) windows -> do
      cardSpecific attrs "followFirstPlayer" ()
      AoaPrelatesMinion <$> runMessage msg attrs
    PassFirstPlayer -> do
      cardSpecific attrs "followFirstPlayer" ()
      AoaPrelatesMinion <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "followFirstPlayer" _ -> do
      first <- selectJust FirstPlayer
      when (first /= minionEngagedIdentity attrs) $ engageMinion (minionId attrs) first
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      let players = [you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows] <> [you | W.EnemyActivation _ you <- windows]
      case toCardCode attrs of
        "45180b" -> for_ (headMay players) $ \you -> do
          allies <- selectList $ AllyControlledBy $ IdentityWithId you
          powers <- for allies $ \aid -> selectCount AllyThwartValue (AllyWithId aid)
          let highest = if null powers then 0 else maximum powers
          chooseOne you [targeting aid $ chooseOne you
            [labeled "$label.aoaDiscardHighestThwartAlly" $ removeFromPlay aid,
             labeled (countLabel "aoaPlaceMainThreat" thw) $ placeThreatOnEach attrs thw MainScheme]
            | (aid, thw) <- zip allies powers, thw == highest]
        "45181b" -> for_ (headMay players) $ \you -> do
          cards <- projectMap PlayerIdentityDeck unDeck you
          for_ (headMay cards) $ attachPlayerCardToMinion $ minionId attrs
        "45182b" -> heal (minionId attrs) 5
        "45183b" -> for_ (headMay players) $ \you -> damageIdentity you attrs $ toDamage 1 FromAbility
        _ -> pure ()
      pure a
    _ -> AoaPrelatesMinion <$> runMessage msg attrs

instance IsMinion AoaPrelatesMinion where
  minionBaseHitPoints _ = perPlayerMinionBaseHitPoints 5
