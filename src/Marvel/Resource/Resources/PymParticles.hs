module Marvel.Resource.Resources.PymParticles (pymParticlesSpent) where

import Marvel.Prelude

import Marvel.Id (CharacterId (..), IdentityId)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Trait

-- | Hero Response: After you spend this card, heal 2 damage from your hero if you are in
-- Giant hero form or draw 1 card if you are in Tiny hero form.
pymParticlesSpent :: (ReverseQueue m) => IdentityId -> m ()
pymParticlesSpent you = do
  giant <- identityMatches (HeroIdentity <> IdentityWithTrait Giant) you
  tiny <- identityMatches (HeroIdentity <> IdentityWithTrait Tiny) you
  when (giant || tiny) $
    chooseOne you $
      [labeled "$label.pymParticlesHeal" (heal (IdentityCharacter you) 2) | giant]
        <> [labeled "$label.pymParticlesDraw" (drawCards you 1) | tiny]
        <> [labeled "$label.noResponse" (pure ())]
