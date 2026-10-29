module Marvel.Hero.Heroes.NebulaHero
  ( nebulaHero, NebulaHero (..), resolveTechnique, techniqueResolved ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait (Trait (Technique))
import Marvel.Window qualified as W

data Protocols = Protocols
  { pending :: [UpgradeId]
  , resolved :: [UpgradeId]
  , collecting :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NebulaHero = NebulaHero (Attrs Hero `With` Protocols)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance IsHero NebulaHero where
  toHeroAttrs (NebulaHero (attrs `With` _)) = attrs

nebulaHero :: HeroCard NebulaHero
nebulaHero = hero (NebulaHero . (`With` Protocols [] [] False)) Cards.nebulaHero
  (HP $ Static 9) (HandSize 5) (Thw 2) (Atk 2) (Def 2)

-- | The card remains responsible for its Special, including status replacement and
-- success reporting. A blanked or no-longer-controlled Technique has no Special here.
resolveTechnique :: (ReverseQueue m) => IdentityId -> UpgradeId -> m ()
resolveTechnique ident uid = do
  present <- selectAny (UpgradeWithId uid <> UpgradeControlledBy (IdentityWithId ident) <> UpgradeWithTrait Technique)
  blanked <- elem Blank <$> getModifiers uid
  when (present && not blanked) $ resolveAs ident (runAbility uid 1)

-- | Report only a Special that actually resolved. Ordinary Gamora/Lethal Intent/
-- Combat Ready resolutions are ignored unless Combat Protocols is collecting them.
techniqueResolved :: (ReverseQueue m) => IdentityId -> UpgradeId -> m ()
techniqueResolved ident uid = cardSpecific ident "nebulaTechniqueResolved" uid

instance HasAbilities NebulaHero where
  getAbilities (NebulaHero (attrs `With` _)) =
    [ label "$label.combatProtocols" $ limitedWindowAbility attrs 1
        (W.TurnBegins $ IdentityWithId ident) ForcedResponse
        (IsSelf <> UpgradeExists (UpgradeWithTrait Technique <> UpgradeControlledBy (IdentityWithId ident)))
        NoCost (runAbility attrs 1)
    ]
   where ident = heroIdentityId attrs

instance RunMessage NebulaHero where
  runMessage msg h@(NebulaHero (attrs `With` meta)) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      upgrades <- selectList (UpgradeWithTrait Technique <> UpgradeControlledBy (IdentityWithId ident))
      cardSpecific attrs "nebulaProtocolNext" ()
      pure $ NebulaHero (attrs `With` Protocols upgrades [] True)
    CardSpecific (isTarget attrs -> True) "nebulaProtocolNext" _ | collecting meta -> do
      live <- filterM (\uid -> selectAny (UpgradeWithId uid <> UpgradeControlledBy (IdentityWithId ident))) (pending meta)
      if null live
        then do
          -- Every Special finishes before any of their passive abilities leave play.
          for_ (resolved meta) \uid -> do
            present <- selectAny (UpgradeWithId uid <> UpgradeControlledBy (IdentityWithId ident))
            when present $ removeFromPlay uid
          pure $ NebulaHero (attrs `With` Protocols [] [] False)
        else do
          chooseOne ident [targeting uid (cardSpecific attrs "nebulaProtocolResolve" uid) | uid <- live]
          pure h
    CardSpecific (isTarget attrs -> True) "nebulaProtocolResolve" (fromJSON -> Success uid)
      | collecting meta && uid `elem` pending meta -> do
          resolveTechnique ident uid
          cardSpecific attrs "nebulaProtocolNext" ()
          pure $ NebulaHero (attrs `With` meta {pending = filter (/= uid) (pending meta)})
    CardSpecific (isTarget attrs -> True) "nebulaTechniqueResolved" (fromJSON -> Success uid)
      | collecting meta -> pure $ NebulaHero (attrs `With` meta {resolved = resolved meta <> [uid | uid `notElem` resolved meta]})
    _ -> NebulaHero . (`With` meta) <$> runMessage msg attrs
   where ident = heroIdentityId attrs
