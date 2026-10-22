module Marvel.Trait.Types where

import Marvel.Prelude

data Trait
  = Aerial
  | Android
  | Armor
  | Assassin
  | Attack
  | Attorney
  | Avenger
  | BlackPanther
  | Brute
  | Condition
  | Criminal
  | Defender
  | Defense
  | Drone
  | Elite
  | Gamma
  | Genius
  | HeroForHire
  | Hydra
  | Item
  | King
  | Kree
  | Location
  | MastersOfEvil
  | Mercenary
  | Persona
  | Shield
  | Skill
  | Soldier
  | Spy
  | Superpower
  | Tactic
  | Tech
  | Thwart
  | Wakanda
  | Weapon
  | Cyborg
  | Vehicle
  | Title
  | Champion
  | Inhuman
  | Bird
  | Robot
  | Preparation
  | Asgard
  | Giant
  | Olympus
  | Mystic
  | Spell
  | Artifact
  | Invocation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)
