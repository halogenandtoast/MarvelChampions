module Marvel.Hero.Heroes.DoctorStrange (
  doctorStrange,
  DoctorStrange (..),
  InvocationDeck (..),
  invocationDeckOf,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Data.Typeable (cast)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Event.Cards qualified as Events
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Cards
import Marvel.Hero.Runner
import Marvel.Identity.Types
import Marvel.Invocation
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Stats
import System.Random.Shuffle (shuffleM)

doctorStrange :: HeroCard DoctorStrange
doctorStrange =
  hero
    (DoctorStrange . (`With` InvocationDeck [] []))
    Cards.doctorStrange
    (HP $ Static 10)
    (HandSize 5)
    (Thw 2)
    (Atk 1)
    (Def 2)

-- | The Invocation deck, its top card first, and its discard pile, newest first. Doctor
-- Strange keeps it for his identity whichever face is up.
data InvocationDeck = InvocationDeck
  { invocationDeck :: [PlayerCard]
  , invocationDiscard :: [PlayerCard]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DoctorStrange = DoctorStrange (Attrs Hero `With` InvocationDeck)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsHero DoctorStrange where
  toHeroAttrs (DoctorStrange (a `With` _)) = a

instance IsRef DoctorStrange where
  toRef = toRef . toHeroAttrs

-- | The player's Invocation deck, if they are Doctor Strange.
invocationDeckOf :: PlayerIdentity -> Maybe InvocationDeck
invocationDeckOf p =
  headMay
    [ deck
    | HeroSide (Hero h) <- HashMap.elems (playerIdentitySides $ toAttrs p)
    , Just (DoctorStrange (_ `With` deck)) <- [cast h]
    ]

printedCost :: PlayerCard -> Natural
printedCost = maybe 0 fromIntegral . cdCost . getCardDef

instance HasAbilities DoctorStrange where
  getAbilities (DoctorStrange (a `With` deck)) =
    [ label "Spell Mastery" $
        ability a 1 Action IsSelf (ExhaustCost <> resources (printedCost top)) (runAbility a 1)
    | top <- take 1 (invocationDeck deck)
    ]
   where
    resources n = if n == 0 then NoCost else MultiResourceCost (replicate (fromIntegral n) Nothing)

-- | Master of the Mystic Arts pays the printed cost of the top Invocation card with its
-- own, both as it is played.
instance HasModifiersFor DoctorStrange where
  getModifiersFor _ target (DoctorStrange (a `With` deck)) | isTarget a target =
    pure
      [ AdditionalResourceCost (CardIs Events.masterOfTheMysticArts) n
      | top <- take 1 (invocationDeck deck)
      , let n = printedCost top
      , n > 0
      ]
  getModifiersFor _ _ _ = pure []

instance RunMessage DoctorStrange where
  runMessage msg (DoctorStrange (a `With` deck)) = case msg of
    RanAbility ident (isTarget a -> True) 1 _ _ -> do
      push $ invocation ident (ResolveTopInvocation False)
      pure $ DoctorStrange (a `With` deck)
    CardSpecific (IdentityRef ident) "invocation" (fromJSON -> Success step)
      | ident == heroIdentityId a -> case step of
          SetUpInvocationDeck -> do
            cards <- for invocationCards \def -> do
              cardId <- getRandom
              pure $ MkPlayerCard cardId def (Just ident) (Just ident)
            settled $ InvocationDeck cards []
          DiscardTopInvocation -> case invocationDeck deck of
            [] -> settled deck
            top : rest -> settled $ InvocationDeck rest (top : invocationDiscard deck)
          ResolveTopInvocation backOnTop -> case invocationDeck deck of
            [] -> settled deck
            top : rest -> do
              pushAll $
                ResolveEventWithoutPlaying ident top
                  : [invocation ident (ReturnToTopOfInvocationDeck top) | backOnTop]
              settled deck {invocationDeck = rest}
          PlaceInInvocationDiscard card ->
            settled deck {invocationDiscard = card : invocationDiscard deck}
          ReturnToTopOfInvocationDeck card ->
            settled $
              InvocationDeck (card : invocationDeck deck) (filter (/= card) $ invocationDiscard deck)
          PlaceTopInvocationUnder target -> case invocationDeck deck of
            [] -> settled deck
            top : rest -> do
              push $ cardSpecific target "invocationUnder" top
              settled deck {invocationDeck = rest}
          ShuffleIntoInvocationDeck card -> do
            cards <- shuffleM (card : invocationDeck deck)
            settled deck {invocationDeck = cards}
    _ -> DoctorStrange . (`With` deck) <$> runMessage msg a
   where
    -- an empty Invocation deck is made again from its discard pile at once
    settled d = case d of
      InvocationDeck [] discard@(_ : _) -> do
        cards <- shuffleM discard
        pure $ DoctorStrange (a `With` InvocationDeck cards [])
      _ -> pure $ DoctorStrange (a `With` d)

invocationCards :: [CardDef]
invocationCards =
  [ Events.crimsonBandsOfCyttorak
  , Events.imagesOfIkonn
  , Events.sevenRingsOfRaggadorr
  , Events.vaporsOfValtorr
  , Events.windsOfWatoomb
  ]
