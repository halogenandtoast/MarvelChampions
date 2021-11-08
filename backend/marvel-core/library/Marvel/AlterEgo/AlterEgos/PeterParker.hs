module Marvel.AlterEgo.AlterEgos.PeterParker where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Attrs
import qualified Marvel.AlterEgo.Cards as Cards
import Marvel.Card.Code
import Marvel.GameValue
import Marvel.Hand
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source

peterParker :: AlterEgoCard PeterParker
peterParker =
  alterEgo PeterParker Cards.peterParker (HP $ Static 10) (HandSize 6) (Rec 3)

newtype PeterParker = PeterParker AlterEgoAttrs
  deriving anyclass IsAlterEgo
  deriving newtype (Show, Eq, HasStartingHP, ToJSON, FromJSON, IsSource, HasCardCode)

instance HasAbilities PeterParker where
  getAbilities a =
    [ label "Scientist" $ limitedAbility
        a
        1
        (PerRound 1)
        Resource
        IsSelf
        (Pay $ ResourcePayment Mental)
    ]

instance RunMessage PeterParker where
  runMessage msg a@(PeterParker attrs) = case msg of
    IdentityMessage ident (SideMessage msg')
      | ident == alterEgoIdentityId attrs -> case msg' of
        SetupIdentity -> do
          pushAll $ map
            (IdentityMessage ident)
            [ShuffleDeck, DrawStartingHand $ alterEgoBaseHandSize attrs]
          pure a
    _ -> pure a
