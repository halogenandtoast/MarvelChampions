module Marvel.Ally.Allies.DumDumDugan (DumDumDugan (..), dumDumDugan) where

import Marvel.Prelude
import Marvel.Id
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Trait (Trait (Shield))
import Marvel.Window qualified as W

-- The bonus belongs to the current power only. Dugan's THW and ATK have different
-- consequential damage values, which the unmodified ally attributes retain.
data Meta = Meta {bonus :: Natural, thwarting :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DumDumDugan = DumDumDugan (Attrs Ally `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsAlly DumDumDugan where
  toAllyAttrs (DumDumDugan (attrs `With` _)) = attrs
instance IsRef DumDumDugan where
  toRef = toRef . toAllyAttrs

instance HasModifiersFor DumDumDugan where
  getModifiersFor _ (AllyRef aid) (DumDumDugan (attrs `With` meta)) | aid == allyId attrs =
    pure [if thwarting meta then ThwartModifier (bonus meta) else AttackModifier (bonus meta)]
  getModifiersFor _ _ _ = pure []

instance HasAbilities DumDumDugan where
  getAbilities (DumDumDugan (attrs `With` _)) =
    [limitedWindowAbility attrs 1
      (W.WindowOneOf [W.AllyAttacked W.When (AllyWithId $ allyId attrs) AnyEnemy, W.AllyThwarted W.When (AllyWithId $ allyId attrs) AnyScheme])
      Interrupt (OwnsThis <> AnyCriteria
        [ SelfMatches (UnexhaustedIdentity <> IdentityWithTrait Shield)
        , AllyExists (UnexhaustedAlly <> AllyControlledBy You <> AllyWithTrait Shield)
        , SupportExists (SupportMatches [UnexhaustedSupport, SupportControlledBy You, SupportWithTrait Shield])
        , UpgradeExists (UnexhaustedUpgrade <> UpgradeControlledBy You <> UpgradeWithTrait Shield) ]) NoCost (runAbility attrs 1)]

dumDumDugan :: AllyCard DumDumDugan
dumDumDugan = ally (DumDumDugan . (`With` Meta 0 False)) Cards.dumDumDugan (Thw 3, 3) (Atk 3, 2) (HP 5)

instance RunMessage DumDumDugan where
  runMessage msg a@(DumDumDugan (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let thw = any (\case W.AllyThwart aid _ -> aid == allyId attrs; _ -> False) windows
      cardSpecific attrs "exhaustShield" (you, thw, 0 :: Natural)
      pure a
    CardSpecific (isTarget attrs -> True) "exhaustShield" (fromJSON -> Success (you, thw :: Bool, n :: Natural)) -> do
      candidates <- readyTraitCardCandidates you Shield
      if n >= 3 || null candidates
        then cardSpecific attrs "applyBonus" (thw, n)
        else chooseOne you $
          [labeled "$label.done" (cardSpecific attrs "applyBonus" (thw, n))]
            <> [targeting target do
                  exhaustTarget target
                  cardSpecific attrs "exhaustShield" (you, thw, n + 1)
               | target <- candidates]
      pure a
    CardSpecific (isTarget attrs -> True) "applyBonus" (fromJSON -> Success (thw, n)) -> do
      addToAllyBasicPower (allyId attrs) n
      pure $ DumDumDugan (attrs `With` Meta n thw)
    CheckWindows windows | any endsUse windows -> pure $ DumDumDugan (attrs `With` Meta 0 False)
    _ -> DumDumDugan . (`With` meta) <$> runMessage msg attrs
   where
    exhaustTarget = \case
      IdentityRef ident -> exhaust ident
      AllyRef ally -> exhaust ally
      SupportRef support -> exhaust support
      UpgradeRef upgrade -> exhaust upgrade
      _ -> pure ()
    endsUse (W.Window W.After (W.AllyAttack aid _)) = aid == allyId attrs
    endsUse (W.Window W.After (W.AllyThwart aid _)) = aid == allyId attrs
    endsUse (W.Window W.After (W.FriendlyCharacterAttack (AllyCharacter aid) _)) = aid == allyId attrs
    endsUse _ = False
