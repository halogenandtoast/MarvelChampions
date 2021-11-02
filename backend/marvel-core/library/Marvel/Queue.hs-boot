module Marvel.Queue where

import {-# SOURCE #-} Marvel.Message

type Queue = [Message]

class HasQueue a
