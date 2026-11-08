-- | The Mad Titan's Shadow's "Max 1 per deck" resources of Adam Warlock's starter deck,
-- each "Hero Response: After you spend this card, ...": offered in hero form only.
module Marvel.Resource.Resources.AdamWarlock (
  audacitySpent,
  determinationSpent,
  innovationSpent,
  preservationSpent,
) where

import Marvel.Prelude

import Marvel.Damage
import Marvel.GameValue
import Marvel.Id (CharacterId (..), IdentityId)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Question (Choice)
import Marvel.Ref

-- | The response, if the player is in hero form and the body has a choice to make.
heroResponse :: (ReverseQueue m) => IdentityId -> [m Choice] -> m ()
heroResponse you choices = do
  hero <- identityMatches HeroIdentity you
  when (hero && notNull choices) $
    chooseOne you (choices <> [labeled "$label.noResponse" (pure ())])

-- | Audacity: deal 1 damage to the villain.
audacitySpent :: (ReverseQueue m) => IdentityId -> m ()
audacitySpent you = do
  villains <- selectAny AnyVillain
  heroResponse you [labeled (countLabel "dealDamageToTheVillain" 1) (withTheVillain you \v -> dealDamage v (IdentityRef you) (toDamage 1 FromAbility)) | villains]

-- | Determination: remove 1 threat from the main scheme.
determinationSpent :: (ReverseQueue m) => IdentityId -> m ()
determinationSpent you = do
  let withThreat = MainScheme <> SchemeWithThreat (GreaterThan $ Static 0)
  available <- selectAny withThreat
  heroResponse you [labeled (countLabel "removeThreatFromMainScheme" 1) (removeThreat you (IdentityRef you) 1 withThreat) | available]

-- | Innovation: heal 1 damage from an ally you control.
innovationSpent :: (ReverseQueue m) => IdentityId -> m ()
innovationSpent you = do
  allies <- filterM (\aid -> characterMatches healableCharacter (AllyCharacter aid)) =<< selectList (AllyControlledBy (IdentityWithId you))
  heroResponse you [targeting (AllyRef a) (heal a 1) | a <- allies]

-- | Preservation: heal 1 damage from your hero.
preservationSpent :: (ReverseQueue m) => IdentityId -> m ()
preservationSpent you = do
  damaged <- identityMatches healableIdentity you
  heroResponse you [labeled (countLabel "healYourIdentity" 1) (heal (IdentityCharacter you) 1) | damaged]
