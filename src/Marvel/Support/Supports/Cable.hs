module Marvel.Support.Supports.Cable where

import Marvel.Prelude
import Data.List ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Status ()
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Keyword qualified as Keyword ()
import {-# SOURCE #-} Marvel.Game ()

import Marvel.Question (runAbility, Choice (Pay))
import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Stats ()
import Marvel.Hand ()

cable40007 :: SupportCard Cable40007
cable40007 = support Cable40007 Cards.cable40007

newtype Cable40007 = Cable40007 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40007 where
  runMessage msg a@(Cable40007 attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ ready (supportId attrs)
    _ -> Cable40007 <$> runMessage msg attrs

cable40008 :: SupportCard Cable40008
cable40008 = support Cable40008 Cards.cable40008

newtype Cable40008 = Cable40008 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40008 where
  runMessage msg a@(Cable40008 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOne you
        [ labeled "$label.drawOneCard" (drawCards you 1)
        , labeled "$label.searchDeck" (search you (SearchIdentityDeckAndDiscardPile you) (CardWithType PlayerSideSchemeType) SearchDrawOne ShuffleBackIn)
        ]
      pure a
    _ -> Cable40008 <$> runMessage msg attrs

cable40021 :: SupportCard Cable40021
cable40021 = support (Cable40021 . (`With` False)) Cards.cable40021

newtype Cable40021 = Cable40021 (Attrs Support `With` Bool)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsSupport Cable40021 where
  toSupportAttrs (Cable40021 (attrs `With` _)) = attrs

instance IsRef Cable40021 where
  toRef = toRef . toSupportAttrs

instance RunMessage Cable40021 where
  runMessage msg a@(Cable40021 (attrs `With` removalScheduled)) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList DamageableEnemy
      schemes <- selectList ThwartableScheme
      fantomex <- selectList (AllyCharacters (AllyWithTitle "Fantomex") <> healableCharacter)
      chooseOne you $
        [targeting (schemeRef sid) $ thwartSchemeAs you sid attrs 1 | sid <- schemes]
        <> [targeting (enemyRef enemy) $ dealDamage enemy attrs (toDamage 1 FromAbility) | enemy <- enemies]
        <> [targeting aid $ heal aid 1 | aid <- fantomex]
      pure a
    _ -> do
      attrs' <- runMessage msg attrs
      exists <- selectAny (AllyWithTitle "Fantomex")
      present <- elem (supportId attrs) <$> selectList (SupportNamed $ cdName $ supportCardDef attrs)
      let mustRemove = present && not exists && not removalScheduled
      when mustRemove $ removeFromPlay attrs
      pure (Cable40021 $ attrs' `With` (removalScheduled || mustRemove))

cable40022 :: SupportCard Cable40022
cable40022 = support Cable40022 Cards.cable40022

newtype Cable40022 = Cable40022 (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40022 where
  runMessage msg (Cable40022 attrs) = runQueueT $ Cable40022 <$> runMessage msg attrs

instance HasAbilities Cable40007 where
  getAbilities a@(Cable40007 attrs) =
    [ limitedWindowAbility a 1 (W.SideSchemeDefeated W.After AnySideScheme) Response OwnsThis NoCost (runAbility a 1)
    | supportExhausted attrs
    ] <> [ability a 2 Resource OwnsThis ExhaustCost (Pay $ ResourcePayment Energy)]

instance HasAbilities Cable40008 where
  getAbilities a@(Cable40008 _) =
    [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]

instance HasAbilities Cable40021 where
  getAbilities a@(Cable40021 (_ `With` _)) =
    [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]

instance HasModifiersFor Cable40022 where
  getModifiersFor _ (AllyRef aid) (Cable40022 attrs) = do
    yours <- selectAny (AllyWithId aid <> AllyControlledBy (IdentityWithId $ supportController attrs))
    if not yours then pure [] else do
      let you = supportController attrs
      heroX <- selectAny (IdentityWithId you <> IdentityWithTrait Trait.XForce)
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      allX <- if heroX then allM (\x -> selectAny (AllyWithId x <> AllyWithTrait Trait.XForce)) allies else pure False
      pure $ if allX then [ThwartModifier 1, SideSchemeThwartConsequentialReduction 1] else []
  getModifiersFor _ _ _ = pure []
