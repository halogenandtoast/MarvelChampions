module Marvel.Trait where

import Marvel.Prelude

data Trait
  = Aerial
  | Android
  | Attack
  | Avenger
  | Brute
  | Condition
  | Criminal
  | Defense
  | Genius
  | HeroForHire
  | Item
  | Location
  | MastersOfEvil
  | Persona
  | Shield
  | Skill
  | Soldier
  | Spy
  | Superpower
  | Tactic
  | Tech
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
