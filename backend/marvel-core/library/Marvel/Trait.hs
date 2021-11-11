module Marvel.Trait where

import Marvel.Prelude

data Trait
  = Aerial
  | Android
  | Armor
  | Attack
  | Avenger
  | Brute
  | Condition
  | Criminal
  | Defense
  | Elite
  | Genius
  | HeroForHire
  | Hydra
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
  | Weapon
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
