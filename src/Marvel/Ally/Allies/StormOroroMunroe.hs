module Marvel.Ally.Allies.StormOroroMunroe where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

stormOroroMunroe :: AllyCard StormOroroMunroe
stormOroroMunroe = ally StormOroroMunroe Cards.stormOroroMunroe (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype StormOroroMunroe = StormOroroMunroe (Attrs Ally)
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor StormOroroMunroe where
  getModifiersFor _ (CardIdRef cid) (StormOroroMunroe a) | cid == CardId (unAllyId $ allyId a) = do
    mutant <- identityMatches (IdentityWithTrait Trait.Mutant) (allyController a)
    xmen <- identityMatches (IdentityWithTrait Trait.XMen) (allyController a)
    let eligible = mutant || xmen
    pure [ResourceCostReduction 1 | eligible]
  getModifiersFor _ _ _ = pure []

instance HasAbilities StormOroroMunroe where
  getAbilities (StormOroroMunroe a) =
    [limitedWindowAbility a 1 (W.AllyThwarted W.When (AllyWithId $ allyId a) (SchemeWithThreat $ GreaterThan $ Static 0))
      Interrupt (OwnsThis <> HasOtherSchemeForThreatMovement) NoCost (runAbility a 1)]

instance RunMessage StormOroroMunroe where
  runMessage msg e@(StormOroroMunroe attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [sid | W.AllyThwart _ sid <- windows] \sid -> do
        schemes <- selectList (NotScheme $ SchemeWithId sid)
        unless (null schemes) $ chooseOne you
          [targeting (schemeRef dest) (moveSchemeThreat attrs sid dest 2) | dest <- schemes]
      pure e
    _ -> StormOroroMunroe <$> runMessage msg attrs
