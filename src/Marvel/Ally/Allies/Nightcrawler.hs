module Marvel.Ally.Allies.Nightcrawler where

import Marvel.Prelude
import Data.List (nub)
import Marvel.Card.Side
import Marvel.Payment
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Stats
import Marvel.Status
import Marvel.Id
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

nightcrawler :: AllyCard Nightcrawler
nightcrawler = ally Nightcrawler Cards.nightcrawler (Thw 2, 1) (Atk 2, 1) (HP 2)

newtype Nightcrawler = Nightcrawler (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Nightcrawler where
  getAbilities (Nightcrawler a) =
    [limitedWindowAbility a 1 (W.CharacterWouldTakeAttackDamage $ CharacterWithTrait Trait.XMen)
      Interrupt OwnsThis (ResourceCost (Just Energy) <> ReturnAllyToHandCost (AllyWithId $ allyId a)) (runAbility a 1)]

instance RunMessage Nightcrawler where
  runMessage msg e@(Nightcrawler attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      let packets = nub
            [(IdentityCharacter ident, damage) | W.IdentityTakeDamage ident damage <- windows, damageAmount damage > 0, damageSource damage `elem` [FromAttack, FromOverkill]]
            <> nub [(AllyCharacter aid, damage) | W.AllyTakeDamage aid damage <- windows, damageAmount damage > 0, damageSource damage `elem` [FromAttack, FromOverkill]]
      eligible <- filterM (characterMatches (CharacterWithTrait Trait.XMen) . fst) packets
      unless (null eligible) $ chooseOrRunOne you
        [targeting (characterRef character) (cardSpecific attrs "preventDamage" (character, damage)) | (character, damage) <- eligible]
      pure e
    CardSpecific (isTarget attrs -> True) "preventDamage" (fromJSON -> Success (character :: CharacterId, damage :: Damage)) -> do
      cancelMatchingMessage $ \case
        IdentityMessage ident (IdentityDamaged _ pending) -> character == IdentityCharacter ident && pending == damage
        AllyMessage aid (AllyDamaged _ pending) -> character == AllyCharacter aid && pending == damage
        _ -> False
      pure e
    _ -> Nightcrawler <$> runMessage msg attrs
