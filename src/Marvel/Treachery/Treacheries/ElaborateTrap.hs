module Marvel.Treachery.Treacheries.ElaborateTrap where
import Marvel.Prelude
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Entity
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as T
import Marvel.Treachery.Types

elaborateTrap :: TreacheryCard ElaborateTrap
elaborateTrap = treachery ElaborateTrap Cards.jubilee47034
newtype ElaborateTrap = ElaborateTrap (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage ElaborateTrap where
  runMessage msg a@(ElaborateTrap attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      traps <- filterM resolvable =<< selectList AnySideScheme
      if null traps
        then discardUntil FromEncounterDeck (CardWithTrait T.Trap <> CardWithType SideSchemeType) attrs
        else for_ traps $ \sid -> cardSpecific (SideSchemeRef sid) "whenDefeated" you
      pure $ ElaborateTrap attrs {treacheryResolver = Just you}
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      you <- maybe getYou pure (treacheryResolver attrs)
      revealEncounterCard you card
      pure a
    _ -> ElaborateTrap <$> runMessage msg attrs

-- Light at the End is also a Trap!, but has no When Defeated ability.
resolvable :: HasGame m => SideSchemeId -> m Bool
resolvable sid = do
  def <- getsGame (`abilityCardDef` SideSchemeRef sid)
  blank <- isBlanked (SideSchemeRef sid)
  pure $ not blank && maybe False ((`elem` ["47031", "47032", "47033"]) . toCardCode) def
