-- | Text for the players goes out as translation keys, as in Arkham: a @$@, a dotted key
-- into the client's locale files, and its variables inline,
--
-- > $error.noSuchScenario scenario=s:"rhino"
--
-- The client looks the key up in the player's language.
module Marvel.I18n (
  I18nVar,
  ikey,
  ikeyWith,
  intVar,
  textVar,
) where

import Data.Text qualified as T
import Marvel.Prelude

data I18nValue = IntValue Int | TextValue Text

type I18nVar = (Text, I18nValue)

ikey :: Text -> Text
ikey key = ikeyWith key []

ikeyWith :: Text -> [I18nVar] -> Text
ikeyWith key vars = T.unwords ("$" <> key : map render vars)
 where
  render (k, IntValue n) = k <> "=i:" <> tshow n
  -- only the quote and backslash are escaped: `show` would mangle accented names
  render (k, TextValue t) = k <> "=s:\"" <> T.concatMap escape t <> "\""
  escape = \case
    '"' -> "\\\""
    '\\' -> "\\\\"
    c -> T.singleton c

intVar :: Text -> Int -> I18nVar
intVar k n = (k, IntValue n)

textVar :: Text -> Text -> I18nVar
textVar k t = (k, TextValue t)
