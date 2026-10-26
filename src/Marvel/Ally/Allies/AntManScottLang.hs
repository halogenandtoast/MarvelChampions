module Marvel.Ally.Allies.AntManScottLang (
  antManScottLang,
  AntManScottLang (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait

antManScottLang :: AllyCard AntManScottLang
antManScottLang = ally AntManScottLang Cards.antManScottLang (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype AntManScottLang = AntManScottLang (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | While you are in Giant hero form, Ant-Man gains the Giant trait and gets +1 ATK.
-- While you are in Tiny hero form, Ant-Man gains the Tiny trait and gets +1 THW.
instance HasModifiersFor AntManScottLang where
  getModifiersFor _ (AllyRef aid) (AntManScottLang a) | aid == allyId a = do
    let you = allyController a
    giant <- identityMatches (HeroIdentity <> IdentityWithTrait Giant) you
    tiny <- identityMatches (HeroIdentity <> IdentityWithTrait Tiny) you
    pure $
      [m | giant, m <- [TraitModifier Giant, AttackModifier 1]]
        <> [m | tiny, m <- [TraitModifier Tiny, ThwartModifier 1]]
  getModifiersFor _ _ _ = pure []

instance RunMessage AntManScottLang where
  runMessage msg (AntManScottLang attrs) = AntManScottLang <$> runMessage msg attrs
