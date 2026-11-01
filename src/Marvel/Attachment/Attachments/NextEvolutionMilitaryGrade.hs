module Marvel.Attachment.Attachments.NextEvolutionMilitaryGrade where

import Marvel.Prelude
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability
    ( AbilityType(Action, HeroResponse, HeroAction),
      HasAbilities(..),
      ability,
      label,
      windowAbility )
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers hiding (UnexhaustedIdentity, UnexhaustedAlly)
import Marvel.Matchers qualified as Matchers ()
import Marvel.Modifier
    ( HasModifiersFor(..),
      Modifier(AttackDamageCap, Blank, AttackModifier, AttackPenalty,
               KeywordModifier) )
import Marvel.NextEvolution.Modulars
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Status
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types


import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MilitaryGradeAttachment = MilitaryGradeAttachment (Attrs Attachment `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ne40090 :: AttachmentCard MilitaryGradeAttachment
ne40090 = attachment (MilitaryGradeAttachment . (`With` (0 :: Natural))) Cards.ne40090

ne40091 :: AttachmentCard MilitaryGradeAttachment
ne40091 = attachment (MilitaryGradeAttachment . (`With` (0 :: Natural))) Cards.ne40091

ne40092 :: AttachmentCard MilitaryGradeAttachment
ne40092 = attachment (MilitaryGradeAttachment . (`With` (0 :: Natural))) Cards.ne40092


instance IsAttachment MilitaryGradeAttachment where
  toAttachmentAttrs (MilitaryGradeAttachment (attrs `With` _)) = attrs
instance IsRef MilitaryGradeAttachment where
  toRef = toRef . toAttachmentAttrs

instance HasAbilities MilitaryGradeAttachment where
  getAbilities a@(MilitaryGradeAttachment (attrs `With` _)) = case toCardCode attrs of
    "40090" -> [label ("$label.neArmament" <> tshow resource) $ windowAbility a idx
      (W.IdentityAttacked W.After AnyIdentity (EnemyWithId enemy)) HeroResponse
      (MultiResourceCost $ replicate 2 $ Just resource) (runAbility a idx)
      | enemy <- maybeToList $ attachmentEnemy attrs, (idx,resource) <- [(1,Physical),(2,Mental),(3,Energy),(4,Wild)]]
    "40091" -> [ability a 10 HeroAction NoCriteria (MultiResourceCost $ replicate 3 Nothing) (runAbility a 10)]
      <> [ability a idx HeroAction NoCriteria (RemoveEnemyStatusCost enemy status) (runAbility a idx)
        | enemy <- maybeToList $ attachmentEnemy attrs, (idx,status) <- [(11,StunnedStatus),(12,ConfusedStatus)]]
    "40092" ->
      [ ability a 20 Action ForAnyPlayer (ExhaustCharacterCost $ CharacterControlledBy You) (runAbility a 20)
      , ability a 21 Action ForAnyPlayer (DamageCost 3) (runAbility a 21)
      ]
    _ -> []

instance HasModifiersFor MilitaryGradeAttachment where
  getBlankingModifiersFor _ (IdentityRef you) (MilitaryGradeAttachment (attrs `With` _))
    | toCardCode attrs == "40092", attachmentIdentity attrs == Just you = pure [Blank]
  getBlankingModifiersFor _ _ _ = pure []
  getPrintedModifiersFor _ target (MilitaryGradeAttachment (attrs `With` _)) = pure $
    [AttackModifier 2 | toCardCode attrs == "40090", Just target == (enemyRef <$> attachmentEnemy attrs)]
    <> [AttackPenalty 1 | toCardCode attrs == "40092", target == maybe GameRef IdentityRef (attachmentIdentity attrs)
      || Just target == (enemyRef <$> attachmentEnemy attrs)]
  getModifiersFor source target a@(MilitaryGradeAttachment (attrs `With` taken)) = do
    blanking <- getBlankingModifiersFor source target a
    pure $ blanking <> case toCardCode attrs of
      "40090" | Just target == (enemyRef <$> attachmentEnemy attrs) -> [KeywordModifier $ Retaliate 2]
      "40091" | Just target == (enemyRef <$> attachmentEnemy attrs) -> [AttackDamageCap $ subtractNatural taken 2]
      _ -> []

instance RunMessage MilitaryGradeAttachment where
  runMessage msg a@(MilitaryGradeAttachment (attrs `With` taken)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      case toCardCode attrs of
        "40090" -> do
          enemies <- selectList EnemyWithHighestAttack
          chooseOrRunOne you [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- enemies]
        "40091" -> do
          enemies <- enemiesWithLeastHP
          unless (null enemies) $ chooseOrRunOne you [targeting (enemyRef enemy) (attachTo aid enemy) | enemy <- enemies]
        "40092" -> attachToIdentity aid you
        _ -> pure ()
      pure a
    RanAbility _ (isTarget attrs -> True) _ _ _ -> a <$ removeFromPlay attrs
    CheckWindows windows | toCardCode attrs == "40091" -> do
      let starts (W.Window W.When (W.IdentityAttackTarget _ enemy)) = Just enemy == attachmentEnemy attrs
          starts (W.Window W.When (W.AllyAttack _ enemy)) = Just enemy == attachmentEnemy attrs
          starts _ = False
          damages = [damageAmount damage | W.Window W.After (W.EnemyTookDamageFrom enemy damage _) <- windows,
            Just enemy == attachmentEnemy attrs, case damageSource damage of FromPlayerAttack _ -> True; FromAllyAttack _ -> True; _ -> False]
          already = if any starts windows then 0 else taken
      attrs' <- runMessage msg attrs
      pure $ MilitaryGradeAttachment (attrs' `With` (already + sum damages))
    _ -> MilitaryGradeAttachment . (`With` taken) <$> runMessage msg attrs
