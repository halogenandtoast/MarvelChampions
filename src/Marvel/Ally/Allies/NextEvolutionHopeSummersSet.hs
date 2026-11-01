module Marvel.Ally.Allies.NextEvolutionHopeSummersSet where

import Marvel.Ally.Runner
import Marvel.Prelude
import Marvel.Status ()
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability ( HasAbilities )
import Marvel.Attack ()
import Marvel.Cost ()
import Marvel.Count
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Difficulty ()
import Marvel.Game.Source ()
import Marvel.Identity.Types ()
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted ()
import Marvel.Modifier ()
import Marvel.NextEvolution.Juggernaut ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Window qualified as W
import Marvel.Card ()
import Marvel.Entity ()
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Types ()


import Marvel.Ref ()
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()


newtype HopeSummersSetAlly = HopeSummersSetAlly (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40130 :: AllyCard HopeSummersSetAlly
ne40130 = ally HopeSummersSetAlly Cards.ne40130 (Thw 0, 0) (Atk 0, 0) (HP 3)


instance HasModifiersFor HopeSummersSetAlly where
  getModifiersFor _ (AllyRef aid) (HopeSummersSetAlly attrs) | aid == allyId attrs = do
    let you = IdentityWithId $ allyController attrs
    attack <- selectCount HeroAttackDamage you
    thwart <- selectCount HeroThwartValue you
    pure [BaseAttack attack, BaseThwart thwart, NotCountedAgainstAllyLimit, ControlledByFirstPlayer]
  getModifiersFor _ _ _ = pure []

instance RunMessage HopeSummersSetAlly where
  runMessage msg a@(HopeSummersSetAlly attrs) = runQueueT $ case msg of
    CheckWindows windows | any left windows -> a <$ gameOver Lost
    _ -> HopeSummersSetAlly <$> runMessage msg attrs
   where
    left (W.Window W.After (W.AllyLeavingPlay _ aid _)) = aid == allyId attrs
    left _ = False
