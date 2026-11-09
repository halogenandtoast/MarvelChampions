module Marvel.AlterEgo.AlterEgos.Cable where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Status ()
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers ()
import Marvel.Payment ()
import Marvel.Query ()
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Keyword qualified as Keyword ()
import {-# SOURCE #-} Marvel.Game ()

import Marvel.Question ( runAbility )
import Marvel.Ability
import Marvel.Modifier
import Marvel.Entity
import Marvel.Card
import Marvel.Ref
import Marvel.Message.Lifted
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Types
import Marvel.Stats
import Marvel.GameValue
import Marvel.Hand
import Marvel.AlterEgo.Runner ()
import Marvel.Obligation.Cards qualified as Obligations

cable40001b :: AlterEgoCard Cable40001B
cable40001b = alterEgo Cable40001B Cards.cable40001b (HP $ Static 12) (HandSize 6) (Rec 4) [Obligations.cable40031]

newtype Cable40001B = Cable40001B (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Cable40001B where
  runMessage msg a@(Cable40001B attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardWithType PlayerSideSchemeType) (SearchTarget $ toRef attrs) ShuffleBackIn
      pure a
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) -> do
      traverse_ (putIntoPlay $ alterEgoIdentityId attrs) cards
      pure a
    _ -> Cable40001B <$> runMessage msg attrs

instance HasAbilities Cable40001B where
  getAbilities a@(Cable40001B _) =
    [ability a 1 Setup NoCriteria NoCost (runAbility a 1)]

instance HasModifiersFor Cable40001B where
  getModifiersFor _ (IdentityRef you) (Cable40001B attrs) | you == alterEgoIdentityId attrs = do
    purged <- any (\c -> toCardCode c == "40006" && pcOwner c == Just you) <$> getPlayerVictoryDisplay
    pure [TraitModifier Trait.Psionic | purged]
  getModifiersFor _ _ _ = pure []
