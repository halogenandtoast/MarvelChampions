module Marvel.Minion.Minions.TigerShark
  ( tigerShark
  , TigerShark(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Window

tigerShark :: MinionCard TigerShark
tigerShark = minion TigerShark Cards.tigerShark (Sch 1) (Atk 3) (HP 6)

newtype TigerShark = TigerShark MinionAttrs
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities TigerShark where
  getAbilities (TigerShark a) =
    [ windowAbility
          a
          1
          (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ toId a) You)
          ForcedResponse
          NoCost
        $ Run [MinionMessage (toId a) MinionBecomeTough]
    ]

instance RunMessage TigerShark where
  runMessage msg e@(TigerShark attrs) = case msg of
    Boost msg' -> case msg' of
      RevealedAsBoost target _ | isTarget attrs target -> do
        villain <- selectJust ActiveVillain
        e <$ push (VillainMessage villain VillainBecomeTough)
      _ -> TigerShark <$> runMessage msg attrs
    _ -> TigerShark <$> runMessage msg attrs
