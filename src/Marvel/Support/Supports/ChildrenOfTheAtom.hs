module Marvel.Support.Supports.ChildrenOfTheAtom where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Deck ()
import Marvel.Discard ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Keyword ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource.Types ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as T
import Marvel.Window ()
import Marvel.Identity.Types ()

import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

childrenOfTheAtom :: SupportCard ChildrenOfTheAtom
childrenOfTheAtom = support ChildrenOfTheAtom Cards.childrenOfTheAtom

newtype ChildrenOfTheAtom = ChildrenOfTheAtom (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ChildrenOfTheAtom where
  getModifiersFor _ target (ChildrenOfTheAtom a) = do
    let memberOfTeam character = anyM (\trait -> selectAny (CharacterWithId character <> CharacterWithTrait trait)) [T.XFactor, T.XForce, T.XMen]
    eligible <- case target of
      IdentityRef you | you == supportController a -> memberOfTeam (IdentityCharacter you)
      AllyRef aid -> do
        ours <- allyMatches (AllyControlledBy $ IdentityWithId $ supportController a) aid
        if ours then memberOfTeam (AllyCharacter aid) else pure False
      _ -> pure False
    pure [TraitModifier trait | eligible, trait <- [T.XFactor, T.XForce, T.XMen]]

instance RunMessage ChildrenOfTheAtom where
  runMessage msg (ChildrenOfTheAtom attrs) = runQueueT $ case msg of
    _ -> ChildrenOfTheAtom <$> runMessage msg attrs
