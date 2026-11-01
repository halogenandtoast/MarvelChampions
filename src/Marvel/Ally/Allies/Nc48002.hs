module Marvel.Ally.Allies.Nc48002 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Boost ()
import Marvel.Phase ()
import Marvel.Ally.Types ()
import Marvel.Minion.Types ()
import Marvel.Nightcrawler.Helpers ()
import Marvel.Minion.Cards ()
import Marvel.SideScheme.Cards ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait ()
import Marvel.Window qualified as W
import Marvel.Ally.Runner
import Marvel.Ally.Cards qualified as Cards

nc48002 :: AllyCard Nc48002
nc48002 = ally Nc48002 Cards.nc48002 (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype Nc48002 = Nc48002 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Nc48002 where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility a 1)]

instance RunMessage Nc48002 where
  runMessage msg a@(Nc48002 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (ExtendedCardMatches [BasicCardMatches (CardWithTitle "Bamf!"), InDeckOf (IdentityWithId you) (BasicCardMatches AnyCard)])
      discards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ CardWithTitle "Bamf!")
      chooseOne you [targeting card (cardSpecific attrs "attach" (you, card)) | card <- cards <> discards]
      when (null (cards <> discards)) $ cardSpecific attrs "damage" ()
      shuffleDeck you
      pure a
    CardSpecific (isTarget attrs -> True) "attach" (fromJSON -> Success (you :: IdentityId, card :: PlayerCard)) -> do
      enemies <- selectList (NotEnemy $ EnemyWithUpgrade $ UpgradeNamed "Bamf!")
      chooseOne you [targeting (enemyRef enemy) (putIntoPlayOn you card (enemyRef enemy)) | enemy <- enemies]
      cardSpecific attrs "damage" ()
      pure a
    CardSpecific (isTarget attrs -> True) "damage" _ -> do
      enemies <- selectList (EnemyWithUpgrade $ UpgradeNamed "Bamf!")
      dealDamageToEach enemies attrs (toDamage 1 FromAbility)
      pure a
    _ -> Nc48002 <$> runMessage msg attrs
