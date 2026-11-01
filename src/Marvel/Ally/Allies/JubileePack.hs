module Marvel.Ally.Allies.JubileePack where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Payment ()
import Marvel.Query
import Marvel.Resource.Types
import Marvel.Window qualified as W

-- Husk's paid bonuses apply to one use; readying waits until that use is complete.
data JubileeAllyMeta = JubileeAllyMeta {huskBonus :: Natural, huskReady :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
newtype JubileeAlly = JubileeAlly (Attrs Ally `With` JubileeAllyMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance IsAlly JubileeAlly where
  toAllyAttrs (JubileeAlly (attrs `With` _)) = attrs
instance IsRef JubileeAlly where
  toRef = toRef . toAllyAttrs
jubilee47002, jubilee47011, jubilee47012, jubilee47018 :: AllyCard JubileeAlly
jubilee47002 = ally (JubileeAlly . (`With` JubileeAllyMeta 0 False)) Cards.jubilee47002 (Thw 1, 1) (Atk 3, 2) (HP 4)
jubilee47011 = ally (JubileeAlly . (`With` JubileeAllyMeta 0 False)) Cards.jubilee47011 (Thw 2, 1) (Atk 2, 1) (HP 3)
jubilee47012 = ally (JubileeAlly . (`With` JubileeAllyMeta 0 False)) Cards.jubilee47012 (Thw 2, 1) (Atk 2, 1) (HP 3)
jubilee47018 = ally (JubileeAlly . (`With` JubileeAllyMeta 0 False)) Cards.jubilee47018 (Thw 1, 1) (Atk 1, 1) (HP 3)

instance HasModifiersFor JubileeAlly where
  getModifiersFor _ (AllyRef aid) (JubileeAlly (attrs `With` meta)) | aid == allyId attrs = do
    confused <- selectList ConfusedEnemy
    pure $ case toCardCode attrs of
      "47002" -> [KeywordModifier Piercing]
      "47011" -> [AttackConsequentialDamageReductionAgainst enemy 1 | enemy <- confused]
      "47012" -> [ThwartModifier $ huskBonus meta, AttackModifier $ huskBonus meta]
      _ -> []
  getModifiersFor _ _ _ = pure []

instance HasAbilities JubileeAlly where
  getAbilities (JubileeAlly (attrs `With` _)) = case toCardCode attrs of
    "47002" -> [limitedWindowAbility attrs 1 (W.IdentityChangedToForm W.After controller) Response
      (OwnsThis <> SelfMatches AlterEgoIdentity <> CharacterExists (CharacterWithId (AllyCharacter $ allyId attrs) <> CharacterWithAnyDamage)) NoCost (runAbility attrs 1)]
    "47012" -> [limitedWindowAbility attrs 1
      (W.WindowOneOf [W.AllyAttacked W.When self AnyEnemy, W.AllyThwarted W.When self AnyScheme])
      Interrupt OwnsThis (UpToResourcesCost 3) (runAbility attrs 1)]
    "47018" -> [limitedWindowAbility attrs 1
      (W.WindowOneOf [W.MakesBasicAttack W.When controller, W.MakesBasicThwart W.When controller, W.MakesBasicDefense W.When controller AnyEnemy, W.UsesRecoveryAt W.When controller])
      Interrupt OwnsThis ExhaustCost (runAbility attrs 1)]
    _ -> []
   where
    controller = IdentityWithId $ allyController attrs
    self = AllyWithId $ allyId attrs

instance RunMessage JubileeAlly where
  runMessage msg a@(JubileeAlly (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ payment -> case toCardCode attrs of
      "47002" -> a <$ heal (allyId attrs) 3
      "47012" -> do
        resources <- paymentResources payment
        let bonus = if Energy `elem` resources then 1 else 0
        addToAllyBasicPower (allyId attrs) bonus
        when (Mental `elem` resources) $ heal (allyId attrs) 1
        pure $ JubileeAlly (attrs `With` JubileeAllyMeta bonus (Physical `elem` resources))
      "47018" -> do
        addToIdentityBasicPower you 1
        pure a
      _ -> pure a
    CheckWindows windows | any endsUse windows -> do
      when (huskReady meta) $ ready (allyId attrs)
      pure $ JubileeAlly (attrs `With` JubileeAllyMeta 0 False)
    _ -> JubileeAlly . (`With` meta) <$> runMessage msg attrs
   where
    endsUse (W.Window W.After (W.AllyAttack aid _)) = aid == allyId attrs
    endsUse (W.Window W.After (W.AllyThwart aid _)) = aid == allyId attrs
    endsUse _ = False
