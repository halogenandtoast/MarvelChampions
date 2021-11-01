module Marvel.Identity.Attrs
  ( module Marvel.Identity.Attrs
  , module X
  ) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import GHC.Generics
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.PlayerCard
import Marvel.Entity
import Marvel.Exception
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id as X
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source

data IdentityAttrs = IdentityAttrs
  { identityAttrsId :: IdentityId
  , identityAttrsCardDef :: CardDef
  , identityAttrsStartingHP :: HP
  , identityAttrsMaxHP :: HP
  , identityAttrsCurrentHP :: HP
  , identityAttrsDeck :: [PlayerCard]
  , identityAttrsPassed :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsSource IdentityAttrs where
  toSource = IdentitySource . toId

defaultAttrs :: IdentityId -> CardDef -> HP -> IdentityAttrs
defaultAttrs ident cardDef hp = IdentityAttrs
  { identityAttrsId = ident
  , identityAttrsCardDef = cardDef
  , identityAttrsStartingHP = hp
  , identityAttrsCurrentHP = hp
  , identityAttrsMaxHP = hp
  , identityAttrsDeck = []
  , identityAttrsPassed = False
  }

deckL :: Lens' IdentityAttrs [PlayerCard]
deckL = lens identityAttrsDeck \m x -> m { identityAttrsDeck = x }

passedL :: Lens' IdentityAttrs Bool
passedL = lens identityAttrsPassed \m x -> m { identityAttrsPassed = x }

takeTurn :: MonadGame env m => IdentityAttrs -> m IdentityAttrs
takeTurn attrs = do
  pushAll $ map (IdentityMessage $ toId attrs) [PlayerTurnOption, CheckIfPassed]
  pure attrs

instance HasAbilities IdentityAttrs where
  getAbilities a = [limitedAbility a (PerTurn 1) 100 IsSelf ChangeForm]

passesUseLimit
  :: IdentityAttrs -> HashMap IdentityId [Ability] -> Ability -> Bool
passesUseLimit x aMap a = case abilityLimit a of
  NoLimit -> True
  PerTurn n -> count (== a) usedAbilities < fromIntegral n
  PerRound n -> count (== a) usedAbilities < fromIntegral n
  where usedAbilities = HashMap.findWithDefault [] (toId x) aMap

passesCriteria :: IdentityAttrs -> Ability -> Bool
passesCriteria x a = case abilityCriteria a of
  IsSelf -> toSource x == abilitySource a

getChoices :: MonadGame env m => IdentityAttrs -> m [Choice]
getChoices attrs = do
  abilities <- getsGame getAbilities
  usedAbilities <- getUsedAbilities
  let
    validAbilities = filter
      (and . sequence [passesUseLimit attrs usedAbilities, passesCriteria attrs]
      )
      abilities
  pure $ map UseAbility validAbilities

runIdentityMessage
  :: MonadGame env m => IdentityMessage -> IdentityAttrs -> m IdentityAttrs
runIdentityMessage msg attrs@IdentityAttrs {..} = case msg of
  BeginTurn -> takeTurn attrs
  CheckIfPassed -> if identityAttrsPassed then pure attrs else takeTurn attrs
  SetDeck cards -> pure $ attrs & deckL .~ cards
  PlayerTurnOption -> do
    choices <- getChoices attrs
    chooseOne (toId attrs) (EndTurn : choices)
    pure attrs
  EndedTurn -> do
    pure $ attrs & passedL .~ True
  ChooseOtherForm -> throwM
    $ UnhandledMessage "ChooseOtherForm must be handled by AlterEgo/Hero"
  ChangedToForm _ ->
    throwM $ UnhandledMessage "ChangedToForm must be handled by PlayerIdentity"

instance RunMessage IdentityAttrs where
  runMessage msg attrs = case msg of
    IdentityMessage ident msg' | ident == toId attrs ->
      runIdentityMessage msg' attrs
    _ -> pure attrs

instance HasStartingHP IdentityAttrs where
  startingHP = identityAttrsStartingHP

instance HasCardCode IdentityAttrs where
  toCardCode = toCardCode . identityAttrsCardDef

instance Entity IdentityAttrs where
  type EntityId IdentityAttrs = IdentityId
  type EntityAttrs IdentityAttrs = IdentityAttrs
  toId = identityAttrsId
  toAttrs = id

class HasIdentityAttrs a where
  identityAttrsL :: (CoerceRole f, Functor f) => (IdentityAttrs -> f IdentityAttrs) -> (a -> f a)

instance HasIdentityAttrs IdentityAttrs where
  identityAttrsL = id

genericToIdentityAttrs
  :: (HasIdentityAttrs' (Rep a), Generic a) => Lens' a IdentityAttrs
genericToIdentityAttrs = lens
  (view identityAttrsL' . from)
  (\m x -> to $ set identityAttrsL' x (from m))

class HasIdentityAttrs' f where
  identityAttrsL' :: Lens' (f p) IdentityAttrs

instance HasIdentityAttrs' f => HasIdentityAttrs' (M1 i c f) where
  identityAttrsL' = lens
    (view identityAttrsL' . unM1)
    (\m x -> M1 $ set identityAttrsL' x (unM1 m))

instance (HasIdentityAttrs' l, HasIdentityAttrs' r) => HasIdentityAttrs' (l :+: r) where
  identityAttrsL' = lens
    (\case
      L1 x -> view identityAttrsL' x
      R1 x -> view identityAttrsL' x
    )
    (\m x -> case m of
      L1 l -> L1 $ set identityAttrsL' x l
      R1 r -> R1 $ set identityAttrsL' x r
    )

instance HasIdentityAttrs c => HasIdentityAttrs' (K1 i c) where
  identityAttrsL' = lens
    (view identityAttrsL . unK1)
    (\m x -> K1 $ set identityAttrsL x (unK1 m))
