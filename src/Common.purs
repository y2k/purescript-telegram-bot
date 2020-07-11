module Common where

import Prelude

import Data.Array.NonEmpty (head)
import Data.DateTime (diff)
import Data.DateTime.Instant (fromDateTime, unInstant, toDateTime, instant)
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Number (fromString) as N
import Data.String (Pattern(..), split)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))

toIntOrZero x = fromString x # maybe 0 identity
tryExtractCommand msg = match (unsafeRegex "/[^@]+" noFlags) msg.text >>= head
unpackButtonData data' = split (Pattern "|") data'
serializeDateTime d = d # fromDateTime # unInstant # unwrap # show
deserializeDateTime str = N.fromString str <#> Milliseconds >>= instant <#> toDateTime
timeInRange to from maxDiff = diff to from < maxDiff
