module Marvel.Treachery.Treacheries.MojoOpening where

import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.MojoOpening
import Marvel.Prelude
import Marvel.Query
import Marvel.Ref
import Marvel.Resource
import Marvel.Trait qualified as Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types
import Marvel.Window qualified as W

-- Defend the Title remembers the attack it starts until its defense resolves.
newtype MojoOpeningTreachery = MojoOpeningTreachery (Attrs Treachery `With` Maybe VillainId)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsTreachery MojoOpeningTreachery where
  toTreacheryAttrs (MojoOpeningTreachery (a `With` _)) = a

mkOpeningTreachery :: Attrs Treachery -> MojoOpeningTreachery
mkOpeningTreachery a = MojoOpeningTreachery (a `With` Nothing)

mojo39009, mojo39010, mojo39011, mojo39017, mojo39019, mojo39020, mojo39021 :: TreacheryCard MojoOpeningTreachery
mojo39009 = treachery mkOpeningTreachery Cards.mojo39009
mojo39010 = treachery mkOpeningTreachery Cards.mojo39010
mojo39011 = treachery mkOpeningTreachery Cards.mojo39011
mojo39017 = treachery mkOpeningTreachery Cards.mojo39017
mojo39019 = treachery mkOpeningTreachery Cards.mojo39019
mojo39020 = treachery mkOpeningTreachery Cards.mojo39020
mojo39021 = treachery mkOpeningTreachery Cards.mojo39021

instance RunMessage MojoOpeningTreachery where
  runMessage msg e@(MojoOpeningTreachery (a `With` defended)) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> case toCardCode a of
      "39009" -> do
        giveStatus you StunnedStatus
        challengers <- challengerRatings
        champion <- championRatings
        let damage = if challengers > champion then 4 else 2
        chooseOne you
          [labeled (countLabel "mojoRatingsToPreventDamage" n) do
             rateChampion n
             damageIdentity you a $ toDamage (damage - n) FromTreachery
          | n <- [0 .. damage]]
        pure e
      "39010" -> do
        hero <- selectAny (IdentityWithId you <> HeroIdentity)
        if hero
          then do
            vid <- selectOne ActiveVillain
            traverse_ (`enemyAttacks` you) vid
            pure $ MojoOpeningTreachery (a `With` vid)
          else e <$ rateChampion 2
      "39011" -> do
        giveStatus you ConfusedStatus
        challengers <- challengerRatings
        champion <- championRatings
        placeThreatOnEach a (if challengers > champion then 4 else 2) EncounterMainScheme
        pure e
      "39017" -> do
        villains <- selectList (VillainWithTrait Trait.Escaped)
        traverse_ flipVillain villains
        revealShow you
        cardSpecific a "returnCornered" ()
        pure e
      "39019" -> do
        cornered <- selectList (VillainWithTrait Trait.Cornered)
        if notNull cornered
          then traverse_ (`placeCounters` 1) cornered
          else do
            canPay <- canAffordCost you (toSource a) (ResourceCost $ Just Mental)
            when canPay $ chooseOne you
              [labeled "$label.mojoLookAtShow" do
                 continuation <- capture $ cardSpecific a "lookAtShow" you
                 payCostThen you a (ResourceCost $ Just Mental) continuation
              ,labeled "$label.done" (pure ())]
        pure e
      "39020" -> do
        shows <- selectList (EnvironmentWithTrait Trait.Show)
        defs <- traverse (\eid -> getsGame (`abilityCardDef` EnvironmentRef eid)) shows
        let sets = catMaybes $ map cdEncounterSet $ catMaybes defs
        for_ (headMay sets) $ \set -> getPlayers >>= traverse_ (\p -> searchAndDealEncounterCard p (CardInEncounterSet set))
        pure e
      "39021" -> do
        swords <- selectAny (AttachmentIs Attachments.mojo39018)
        if swords
          then withTheVillain you (`enemyAttacks` you)
          else searchAndRevealEncounterCard you (CardIs Attachments.mojo39018)
        pure e
      _ -> pure e
    CardSpecific (isTarget a -> True) "returnCornered" _ -> do
      let card = MkEncounterCard (CardId $ unTreacheryId $ treacheryId a) (getCardDef a)
      -- Remove the resolving entity without discarding its card: the physical
      -- Cornered! card returns directly to the show deck, which has no discard.
      removeFromGame a
      fromDeck showDeck $ shuffleIntoEncounterDeck [card]
      pure e
    CardSpecific (isTarget a -> True) "lookAtShow" (fromJSON -> Success you) -> do
      pile <- getPile showDeck
      for_ (headMay $ pileDeck pile) $ \card -> chooseOneSearchedIn you "$label.mojoPlaceShow" [("$label.mojoShowDeck", [EncounterCard card])]
        [labeled "$label.mojoKeepShowOnTop" (pure ())
        ,labeled "$label.mojoMoveShowToBottom" (fromDeck showDeck $ putOnBottomOfEncounterDeck [card])]
      pure e
    CheckWindows windows | toCardCode a == "39010", Just vid <- defended -> do
      let success = any (\case W.Window W.After (W.DefendedWithoutDamage _ (EnemyVillainId enemy)) -> enemy == vid; _ -> False) windows
      when success $ rateChallengers 2
      pure $ if success then MojoOpeningTreachery (a `With` Nothing) else e
    TreacheryMessage tid (ResolvedTreachery _) | tid == treacheryId a, toCardCode a == "39017" -> pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      getBoostedPlayer enemy >>= (`giveStatus` ConfusedStatus)
      pure e
    _ -> MojoOpeningTreachery . (`With` defended) <$> runMessage msg a
