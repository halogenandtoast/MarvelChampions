{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.JessicaJones.Helpers where
import Marvel.Prelude
import Marvel.Entity
import Marvel.Projection
import Marvel.Identity.Types
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.Hero.Runner qualified as Hero
import Data.HashMap.Strict qualified as HashMap
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers qualified as M
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Question
import Marvel.Status
import Marvel.Trait qualified

aliasFor :: HasGame m => IdentityId -> m (Maybe SupportId)
aliasFor you = selectOne (SupportNamed "Alias Investigations" <> SupportControlledBy (IdentityWithId you))

evidenceCount :: HasGame m => IdentityId -> m Natural
evidenceCount you = aliasFor you >>= maybe (pure 0) getCountersOn

addEvidence :: ReverseQueue m => IdentityId -> Natural -> m ()
addEvidence you n = aliasFor you >>= traverse_ (`placeCounters` n)

spendEvidence :: ReverseQueue m => IdentityId -> Natural -> m ()
spendEvidence you n = aliasFor you >>= traverse_ (`removeCounters` n)

toughCount :: HasGame m => IdentityId -> m Natural
toughCount = characterToughCount . IdentityCharacter

chooseFriendlyTough :: ReverseQueue m => IdentityId -> CharacterMatcher -> m ()
chooseFriendlyTough you matcher = do
  targets <- selectList (matcher <> CharacterControlledBy AnyIdentity <> CharacterCanGainTough)
  chooseOne you [targeting (characterRef c) (giveStatus c ToughStatus) | c <- targets]

readyControlled :: ReverseQueue m => IdentityId -> m ()
readyControlled you = do
  targets <- selectList (CharacterControlledBy (IdentityWithId you) <> CharacterCanBeReadied)
  chooseOne you [targeting (characterRef c) (readyCharacter c) | c <- targets]

readyCharacter :: ReverseQueue m => CharacterId -> m ()
readyCharacter = \case
  IdentityCharacter you -> ready you
  AllyCharacter ally -> ready ally
  _ -> pure ()

teamCharacters :: HasGame m => [Text] -> m [CharacterId]
teamCharacters names = concat <$> traverse (\name -> do
  heroes <- selectList (IdentityWithTitle name)
  allies <- selectList (AllyNamedAs name)
  pure $ map IdentityCharacter heroes <> map AllyCharacter allies) names

-- A Joys of Life mode needs a character to ready before its exhaust cost
-- may be paid. Its actual exhaust is paid by ActiveCost, including Alliance consent.
joysCivilian :: CharacterMatcher
joysCivilian = IdentityCharacters (AlterEgoIdentity <> IdentityWithTrait Marvel.Trait.Civilian)

joysHeroOrAlly :: CharacterMatcher
joysHeroOrAlly = CharacterOneOf [IdentityCharacters HeroIdentity, AllyCharacters AnyAlly]

joysExhaustMatcher :: HasGame m => m CharacterMatcher
joysExhaustMatcher = do
  readyHeroOrAlly <- selectAny (joysHeroOrAlly <> CharacterCanBeReadied)
  readyCivilian <- selectAny (joysCivilian <> CharacterCanBeReadied)
  pure $ CharacterOneOf ([joysCivilian | readyHeroOrAlly] <> [joysHeroOrAlly | readyCivilian])

heroDefense :: (HasGame m, Projection m PlayerIdentity) => IdentityId -> m Natural
heroDefense you = do
  side <- project PlayerIdentitySide you
  sides <- project PlayerIdentitySides you
  case HashMap.lookup side sides of
    Just (HeroSide hero) -> Hero.getModifiedDefense (toAttrs hero)
    _ -> pure 0

cancelAllyDefeat :: ReverseQueue m => AllyId -> m ()
cancelAllyDefeat aid = do
  cancelMatchingMessage $ \case
    PrepareAllyDepartureByDefeat other _ -> aid == other
    _ -> False
  replaceMatchingMessage (\case
    CheckWindows windows -> [CheckWindows $ filter (not . defeatedWindow) windows]
    message -> [message]) $ \case
      CheckWindows windows -> any defeatedWindow windows
      _ -> False
 where
  defeatedWindow (W.Window _ kind) = case kind of
    W.DefeatedAlly other _ -> aid == other
    W.AllyCardDefeated _ card -> pcCardId card == CardId (unAllyId aid)
    W.AllyCardDefeatedByConsequentialDamage _ card -> pcCardId card == CardId (unAllyId aid)
    _ -> False

alterEgoRecovery :: (HasGame m, Projection m PlayerIdentity) => IdentityId -> m Natural
alterEgoRecovery you = do
  side <- project PlayerIdentitySide you
  sides <- project PlayerIdentitySides you
  case HashMap.lookup side sides of
    Just (AlterEgoSide ae) -> AlterEgo.getModifiedRecovery (toAttrs ae)
    _ -> pure 0
