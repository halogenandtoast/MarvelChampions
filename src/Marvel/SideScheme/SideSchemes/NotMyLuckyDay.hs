module Marvel.SideScheme.SideSchemes.NotMyLuckyDay where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Domino ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Identity.Types
import Marvel.Matchers ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

notMyLuckyDay :: SideSchemeCard NotMyLuckyDay
notMyLuckyDay = sideScheme NotMyLuckyDay Cards.notMyLuckyDay (Static 3)
newtype NotMyLuckyDay = NotMyLuckyDay (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage NotMyLuckyDay where
  runMessage msg a@(NotMyLuckyDay attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players $ cardSpecific attrs "choose"
      pure a
    CardSpecific (isTarget attrs -> True) "choose" (fromJSON -> Success you) -> do
      chooseOne you
        [ labeled "$label.dominoTakeDamage" (damageIdentity you attrs $ toDamage 1 FromAbility)
        , labeled "$label.dominoPlaceThreat" (placeThreat (sideSchemeId attrs) 2)
        ]
      pure a
    _ -> NotMyLuckyDay <$> runMessage msg attrs
