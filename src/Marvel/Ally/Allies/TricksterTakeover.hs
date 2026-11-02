module Marvel.Ally.Allies.TricksterTakeover where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.TricksterTakeover.Cards qualified as Cards
import Marvel.Window qualified as W
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

newtype TTAlly = TTAlly (Attrs Ally)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsAlly

tt55063 :: AllyCard TTAlly
tt55063 = ally TTAlly Cards.tt55063 (Thw 1, 2) (Atk 1, 2) (HP 3)

tt55064 :: AllyCard TTAlly
tt55064 = ally TTAlly Cards.tt55064 (Thw 2, 1) (Atk 3, 2) (HP 5)

tt55065 :: AllyCard TTAlly
tt55065 = ally TTAlly Cards.tt55065 (Thw 1, 1) (Atk 0, 1) (HP 3)

tt55066 :: AllyCard TTAlly
tt55066 = ally TTAlly Cards.tt55066 (Thw 3, 2) (Atk 1, 1) (HP 4)

instance HasModifiersFor TTAlly where
  getModifiersFor _ target (TTAlly a) = pure $ if target == toRef a then
    [NotCountedAgainstAllyLimit] <> (if toCardCode a == "55064" then [KeywordModifier Overkill, KeywordModifier Piercing] else []) else []

instance HasAbilities TTAlly where
  getAbilities (TTAlly a) = case toCardCode a of
    "55063" ->
      [windowAbility a 1 (W.AllyAttacked W.When (AllyWithId $ allyId a) AnyEnemy) Interrupt NoCost (runAbility a 1)
      ,windowAbility a 1 (W.AllyThwarted W.When (AllyWithId $ allyId a) AnyScheme) Interrupt NoCost (runAbility a 1)]
    "55065" -> [windowAbility a 2 (W.AllyAttacked W.When (AllyWithId $ allyId a) AnyEnemy) Interrupt NoCost (runAbility a 2)]
    "55066" -> [windowAbility a 3 (W.AllyEntersPlay W.After $ AllyWithId $ allyId a) Response NoCost (runAbility a 3)]
    _ -> []

instance RunMessage TTAlly where
  runMessage msg e@(TTAlly a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      upgrades <- selectList (UpgradeControlledBy (IdentityWithId you) <> UnexhaustedUpgrade)
      supports <- selectList (SupportMatches [SupportControlledBy (IdentityWithId you), UnexhaustedSupport])
      chooseOne you $ [targeting uid (exhaust uid >> cardSpecific a "ttAbsorbCost" (UpgradeRef uid)) | uid <- upgrades]
        <> [targeting sid (exhaust sid >> cardSpecific a "ttAbsorbCost" (SupportRef sid)) | sid <- supports]
      pure e
    CardSpecific (isTarget a -> True) "ttAbsorbCost" (fromJSON -> Success target) -> do
      cost <- getsGame $ maybe 0 (fromMaybe 0 . cdCost . getCardDef) . (`cardInPlay` target)
      addToAllyBasicPower (allyId a) (fromIntegral $ max 0 $ min 3 cost)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      schemes <- selectList ThwartableScheme
      amounts <- traverse (\sid -> (sid,) <$> getSchemeThreatAmount sid) schemes
      let before = sum $ map snd amounts
      for_ amounts $ \(sid,n) -> when (n > 0) $ removeSchemeThreat a sid 1
      cardSpecific a "ttWhirlwindBonus" (map fst amounts, before)
      pure e
    CardSpecific (isTarget a -> True) "ttWhirlwindBonus" (fromJSON -> Success (schemes :: [SchemeId], before :: Natural)) -> do
      after <- sum <$> traverse getSchemeThreatAmount schemes
      addToAllyBasicPower (allyId a) (subtractNatural after before)
      pure e
    RanAbility you (isTarget a -> True) 3 _ _ -> do
      search you (SearchIdentityDeck you $ TopOfDeck 5) (CardWithResource Energy) SearchDrawOne ShuffleBackIn
      pure e
    _ -> TTAlly <$> runMessage msg a
