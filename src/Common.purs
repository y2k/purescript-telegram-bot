module Common where

import Prelude

import Data.Array (uncons)
import Data.Array.NonEmpty (head)
import Data.DateTime (diff, DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant, toDateTime, instant)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.Number (fromString) as N
import Data.String (Pattern(..), split)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))

toIntOrZero x = fromString x # maybe 0 identity

tryExtractCommand msg = 
  case toMaybe msg.text of
    Just text -> match (unsafeRegex "/[^@]+" noFlags) text >>= head
    Nothing -> Nothing

serializeDateTime d = d # fromDateTime # unInstant # unwrap # show

deserializeDateTime str = N.fromString str <#> Milliseconds >>= instant <#> toDateTime

timeInRange to from maxDiff = diff to from < maxDiff

packData :: String -> { id :: Int } -> String -> DateTime -> String
packData cmd from tag now = "4|" <> cmd <> "|" <> (show from.id) <> "|" <> tag <> "|" <> (serializeDateTime now)

unpackData data' = 
  case uncons $ split (Pattern "|") data' of
    Just { head: "4", tail } -> tail
    _ -> []

millisecondsFromSeconds timeout = Milliseconds (1_000.0 * (toNumber timeout))
