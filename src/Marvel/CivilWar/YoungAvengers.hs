module Marvel.CivilWar.YoungAvengers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.MightyAvengers qualified as Shared
import Marvel.CivilWar.ShieldOps qualified as SharedSurge
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types

newtype YoungAvenger = YoungAvenger (Attrs Minion)
  deriving anyclass (IsMinion,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

youngAvengers :: [MinionCard YoungAvenger]
youngAvengers = map make [C.cw57055,C.cw57056,C.cw57057]
 where
  make def = let (sch,atk,hp) = C.printedMinionStats (toCardCode def)
    in minion YoungAvenger def (Sch sch) (Atk atk) (HP hp)

instance HasModifiersFor YoungAvenger where
  getModifiersFor _ target (YoungAvenger a)
    | toCardCode a == "57056",isTarget a target = pure [KeywordModifier K.Piercing,KeywordModifier K.Ranged]
  getModifiersFor _ _ _ = pure []

instance RunMessage YoungAvenger where
  runMessage msg e@(YoungAvenger a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      case toCardCode a of
        "57055" -> enemyLeaderFor you >>= traverse_ enemyBecomesTough
        "57056" -> discardableControlledCardsMatching you (CardWithType UpgradeType) >>= chooseAndDiscardControlled you
        "57057" -> discardableControlledCardsMatching you (CardWithType SupportType) >>= chooseAndDiscardControlled you
        _ -> pure ()
      pure e
    _ -> YoungAvenger <$> runMessage msg a

-- These printings have exactly the same rules text as their earlier handlers.
teenageSuperheroes :: TreacheryCard Shared.MightyAvengersTreachery
teenageSuperheroes = treachery Shared.MightyAvengersTreachery C.cw57058

youngAvengersScheme :: SideSchemeCard SharedSurge.HomelandSecurity
youngAvengersScheme = sideScheme SharedSurge.HomelandSecurity C.cw57059 (Static 3)
