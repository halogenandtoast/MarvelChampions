module Marvel.NextEvolution.Sinister where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

sinister :: HasGame m => m (Maybe VillainId)
sinister = villainNamed "Mister Sinister"

superpowers :: HasGame m => VillainId -> m Natural
superpowers vid = fromIntegral . length <$> selectList (AttachmentOnEnemy (EnemyVillainId vid) <> AttachmentWithTrait Trait.Superpower)

sinisterHas :: HasGame m => Trait.Trait -> m Bool
sinisterHas trait = sinister >>= maybe (pure False) (\vid -> selectAny $ VillainWithId vid <> VillainWithTrait trait)

attachToTheVillain :: ReverseQueue m => AttachmentId -> m ()
attachToTheVillain aid = selectOne ActiveVillain >>= traverse_ (\vid -> attachTo aid vid >> attachedTo vid aid)

boostActivationStats :: ReverseQueue m => VillainId -> Natural -> m ()
boostActivationStats vid n = do
  createEffect Minions.ne40145 (VillainRef vid) (EnemyEntity $ EnemyWithId $ EnemyVillainId vid)
  cardSpecific (VillainRef vid) "sinisterBoostAmount" n
