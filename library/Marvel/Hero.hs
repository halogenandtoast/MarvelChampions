module Marvel.Hero where

import Marvel.Prelude

import Marvel.Hp
import Marvel.Identity.Attrs

data Hero = SpiderMan' SpiderMan | CaptainMarvel' CaptainMarvel
  deriving stock (Show, Eq, Generic)

instance HasStartingHP Hero where
  startingHP = defaultHasStartingHP

instance HasIdentityAttrs Hero where
  toIdentityAttrs = genericToIdentityAttrs

newtype SpiderMan = SpiderMan HeroAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

newtype CaptainMarvel = CaptainMarvel HeroAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

newtype HeroAttrs = HeroAttrs IdentityAttrs
  deriving newtype (Show, Eq, HasStartingHP, HasIdentityAttrs)

