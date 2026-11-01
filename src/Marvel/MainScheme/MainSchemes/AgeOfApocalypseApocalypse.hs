module Marvel.MainScheme.MainSchemes.AgeOfApocalypseApocalypse where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Matchers qualified as Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Scenario.AgeOfApocalypseApocalypse
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaApocalypseMainScheme = AoaApocalypseMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

theAgeOfApocalypse :: MainSchemeCard AoaApocalypseMainScheme
theAgeOfApocalypse = mainScheme AoaApocalypseMainScheme Cards.theAgeOfApocalypse (Static 0) (PerPlayer 1) (PerPlayer 1)

instance IsMainScheme AoaApocalypseMainScheme where
  mainSchemeModifiersFor _ target (AoaApocalypseMainScheme attrs)
    | isTarget attrs target = (\n -> [TargetThreatModifier n]) <$> apocalypseScalarHp
  mainSchemeModifiersFor _ _ _ = pure []
  mainSchemeAbilities a =
    [limitedWindowAbility a 1 (W.VillainWouldBeDefeatedMatching AnyVillain) ForcedInterrupt
      (NotCriteria $ AttachmentExists $ AttachmentIs Attachments.noLongerWorthy) NoCost (runAbility a 1)
    ,limitedWindowAbility a 2 (W.MainSchemeWouldComplete Matchers.MainScheme) ForcedInterrupt NoCriteria NoCost (runAbility a 2)]
instance RunMessage AoaApocalypseMainScheme where
  runMessage msg a@(AoaApocalypseMainScheme attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      searchAndRevealEncounterCard first $ CardIs SideSchemes.heartOfTheEmpire
      revealRandomPrelate first
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      current <- apocalypse
      for_ current $ \vid -> do
        cancelVillainDefeat vid
        attachments <- selectList $ AttachmentOnEnemy $ EnemyVillainId vid
        traverse_ removeFromPlay attachments
        cardSpecific vid "aoaApocalypseHealAll" ()
      n <- apocalypseScalarHp
      mainSchemeStep (mainSchemeId attrs) $ MainSchemeThreatRemoved (toSource attrs) n
      pure a
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      cancelMatchingMessage $ \case
        CardSpecific target "resolveThreatPlacement" _ -> isTarget attrs target
        AdvanceMainScheme sid -> sid == mainSchemeId attrs
        _ -> False
      current <- apocalypse
      for_ current $ \vid -> cardSpecific vid "aoaApocalypseAdvanceStage" ()
      pure $ AoaApocalypseMainScheme attrs {mainSchemeThreat = 0}
    _ -> AoaApocalypseMainScheme <$> runMessage msg attrs
