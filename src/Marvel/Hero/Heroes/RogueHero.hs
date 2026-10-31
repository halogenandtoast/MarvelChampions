module Marvel.Hero.Heroes.RogueHero where

import Marvel.Prelude
import Marvel.Card.Code
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Window qualified as W

rogueHero :: HeroCard RogueHero
rogueHero = hero RogueHero Cards.rogueHero (HP $ Static 11) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

newtype RogueHero = RogueHero (Attrs Hero)
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RogueHero where
  getAbilities a =
    [ label "$label.skinContact" $ limitedAbility a 1 (PerRound 1) Action (IsSelf <> CanFindTouched) NoCost (runAbility a 1)
    , limitedWindowAbility a 2 (W.PhaseBegins W.After PlayerPhase) ForcedResponse NoCriteria NoCost (runAbility a 2)
    ]

-- Current RR characters include alter-egos as well as heroes and allies/enemies.
rogueContactTargets :: HasGame m => IdentityId -> m [CharacterId]
rogueContactTargets you = do
  characters <- selectList (CharacterOtherThan $ IdentityCharacter you)
  filterM (fmap (notElem CannotHaveAttachedPlayerCards) . getModifiers . characterRef) characters

-- A type change does not make a character leave play.
rogueCharacterInPlay :: HasGame m => CharacterId -> m Bool
rogueCharacterInPlay character = do
  present <- selectAny (CharacterWithId character)
  if present then pure True else case character of
    MinionCharacter m -> selectAny (CharacterWithId $ AllyCharacter $ AllyId $ unMinionId m)
    AllyCharacter a -> selectAny (CharacterWithId $ MinionCharacter $ MinionId $ unAllyId a)
    _ -> pure False

copyRogueTraits :: (ReverseQueue m, IsRef source) => source -> IdentityId -> CharacterId -> m ()
copyRogueTraits source you host = whenM (rogueCharacterInPlay host) do
  createEffect Cards.rogueHero source (IdentityEntity $ IdentityWithId you)
  cardSpecific source "rogueCopiedTraits" host

instance RunMessage RogueHero where
  runMessage msg h@(RogueHero attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- rogueContactTargets you
      chooseOne you [targeting (characterRef character) do
        findAndAttachTouched you character
        copyRogueTraits attrs you character
        | character <- characters]
      pure h
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> h <$ setAsideTouched (heroIdentityId attrs)
    _ -> RogueHero <$> runMessage msg attrs

newtype RogueHeroEffect = RogueHeroEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

rogueHeroEffect :: CardEffect RogueHeroEffect
rogueHeroEffect = effectWith RogueHeroEffect Cards.rogueHero (endsL ?~ DisableAtEndOfRound)

instance RunMessage RogueHeroEffect where
  runMessage msg e@(RogueHeroEffect attrs) = runQueueT $ case msg of
    CardSpecific target "rogueCopiedTraits" (fromJSON -> Success host)
      | target == refocus (effectSource attrs), null (effectModifiers attrs) ->
          pure $ RogueHeroEffect attrs {effectModifiers = [CopyTraitsFrom host]}
    CardSpecific (isTarget attrs -> True) "rogueCheckHost" _ -> do
      for_ [host | CopyTraitsFrom host <- effectModifiers attrs] $ \host -> do
        present <- rogueCharacterInPlay host
        unless present $ disableEffect (effectId attrs)
      pure e
    -- Check after departure resolves, so replacements that keep a host in play
    -- do not incorrectly terminate the lasting effect.
    RemoveFromPlay _ -> checkHost
    RemoveFromPlayAfterWindow _ -> checkHost
    ReturnToHand _ -> checkHost
    RemoveFromGame _ -> checkHost
    AddToVictoryDisplay _ -> checkHost
    _ -> RogueHeroEffect <$> runMessage msg attrs
   where
    checkHost = e <$ cardSpecific attrs "rogueCheckHost" ()
