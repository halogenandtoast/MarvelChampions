module Marvel.Attachment.Attachments.MojoGenres where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Discard
import Marvel.Entity
import Marvel.Effect.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

newtype MojoGenreAttachment = MojoGenreAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39048, mojo39067 :: AttachmentCard MojoGenreAttachment
mojo39048 = attachment MojoGenreAttachment Cards.mojo39048
mojo39067 = attachment MojoGenreAttachment Cards.mojo39067

instance HasModifiersFor MojoGenreAttachment where
  getModifiersFor _ target (MojoGenreAttachment a)
    | toCardCode a == "39067", Just enemy <- attachmentEnemy a, target == enemyRef enemy = do
        n <- fromIntegral <$> getPlayerCount
        pure [HitPointModifier $ 3 * n]
  getModifiersFor _ _ _ = pure []

instance HasAbilities MojoGenreAttachment where
  getAbilities (MojoGenreAttachment a) =
    [limitedWindowAbility a 1 (W.MakesBasicAttack W.When $ IdentityWithId you) HeroInterrupt (SelfMatches $ IdentityWithId you) (ExactCountersCost 1) (runAbility a 1) | toCardCode a == "39048", Just you <- [attachmentIdentity a], attachmentCounters a > 0]
    <> [windowAbility a 2 (W.MinionDefeated W.When $ MinionWithId mid) ForcedInterrupt NoCost (runAbility a 2) | toCardCode a == "39067", Just (EnemyMinionId mid) <- [attachmentEnemy a]]

instance RunMessage MojoGenreAttachment where
  runMessage msg e@(MojoGenreAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      case toCardCode attrs of
        "39048" -> do
          canPay <- canAffordCost you (toSource attrs) (ResourceCost Nothing)
          if canPay then chooseOne you
            [ labeled "$label.mojoTakeStakes" do
                payForTreachery you (ResourceCost Nothing)
                attachToIdentity aid you
                placeCounters attrs 3
            , labeled "$label.done" (removeFromPlay attrs)
            ] else removeFromPlay attrs
        "39067" -> do
          minions <- selectList MinionWithHighestPrintedHitPoints
          if null minions then removeFromPlay attrs >> surge you
            else chooseOrRunOne you [targeting mid (attachTo aid mid) | mid <- minions]
        _ -> pure ()
      pure e
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower you 1
      createEffect Cards.mojo39048 attrs (IdentityEntity $ IdentityWithId you)
      when (attachmentCounters attrs == 0) $ removeFromPlay attrs
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      players <- getPlayers
      for_ players $ \you -> do
        cards <- unDiscard <$> project PlayerIdentityDiscard you
        unless (null cards) $ chooseOne you [targeting c (addToHand you c) | c <- cards]
      pure e
    _ -> MojoGenreAttachment <$> runMessage msg attrs

-- The attack retains piercing after its last stake discards the attachment.
newtype MojoStakeEffect = MojoStakeEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

mojoStakeEffect :: CardEffect MojoStakeEffect
mojoStakeEffect = effectWith MojoStakeEffect Cards.mojo39048 $
  (modifiersL .~ [KeywordModifier Piercing]) . (endsL ?~ DisableAtEndOfTurn)

instance RunMessage MojoStakeEffect where
  runMessage msg e@(MojoStakeEffect attrs) = runQueueT $ case msg of
    CheckWindows windows | any ended windows -> e <$ disableEffect (effectId attrs)
    _ -> MojoStakeEffect <$> runMessage msg attrs
   where
    ended = \case W.Window W.After (W.MadeBasicAttack _) -> True; _ -> False
