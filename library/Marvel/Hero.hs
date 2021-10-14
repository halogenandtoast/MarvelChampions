module Marvel.Hero where

import Marvel.Prelude

import Marvel.Hp
import Marvel.Message
import Marvel.Identity.Attrs

data Hero = SpiderMan' SpiderMan | CaptainMarvel' CaptainMarvel
  deriving stock (Show, Eq, Generic)

instance RunMessage Hero where
  runMessage = genericRunMessage

instance HasStartingHP Hero where
  startingHP = defaultHasStartingHP

instance HasIdentityAttrs Hero where
  toIdentityAttrs = genericToIdentityAttrs

newtype SpiderMan = SpiderMan HeroAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

instance RunMessage SpiderMan where
  runMessage msg (SpiderMan attrs) = SpiderMan <$> runMessage msg attrs

newtype CaptainMarvel = CaptainMarvel HeroAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

instance RunMessage CaptainMarvel where
  runMessage msg (CaptainMarvel attrs) = CaptainMarvel <$> runMessage msg attrs

newtype HeroAttrs = HeroAttrs IdentityAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

instance RunMessage HeroAttrs where
  runMessage msg (HeroAttrs identityAttrs) = HeroAttrs <$> runMessage msg identityAttrs

