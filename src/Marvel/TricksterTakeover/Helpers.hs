module Marvel.TricksterTakeover.Helpers where

import Marvel.Identity.Types (PlayerIdentity, Field(PlayerIdentityDamage))
import Marvel.Projection
import Marvel.Prelude
import Marvel.Card
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

charm :: ReverseQueue m => IdentityId -> m ()
charm you = cardSpecific GameRef "ttCharm" you

shatter :: ReverseQueue m => Natural -> m ()
shatter n = selectOne ActiveVillain >>= traverse_ (`placeCounters` n)

synergy :: ReverseQueue m => CardCode -> Natural -> m ()
synergy code n = cardSpecific GameRef "ttSynergy" (code, n)

chooseSynergy :: ReverseQueue m => IdentityId -> Natural -> m ()
chooseSynergy you n = do
  environments <- selectList (EnvironmentWithTrait Trait.Synergy)
  chooseOne you [targeting eid (cardSpecific eid "ttAddSynergy" n) | eid <- environments]

expert :: HasGame m => m Bool
expert = (== Expert) <$> getDifficulty

isTreachery :: HasCardDef a => a -> Bool
isTreachery = (== TreacheryType) . cdCardType . getCardDef

mainThreat :: (ReverseQueue m, IsRef a) => a -> Natural -> m ()
mainThreat source n = selectOne EncounterMainScheme >>= traverse_ (\mid -> placeThreatOn source mid n)

enthralled :: HasGame m => IdentityId -> m Bool
enthralled you = selectAny (IdentityWithId you <> IdentityWithTrait Trait.Enthralled)

discardControlledMatching :: ReverseQueue m => IdentityId -> CardMatcher -> m ()
discardControlledMatching you matcher = discardableControlledCardsMatching you matcher >>= chooseAndDiscardControlled you

getIdentityDamage :: Projection m PlayerIdentity => IdentityId -> m Natural
getIdentityDamage = project PlayerIdentityDamage
