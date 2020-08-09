module Common where

import Prelude

import Data.Array as A
import Data.Array.NonEmpty (head, toArray)
import Data.DateTime (diff, DateTime)
import Data.DateTime.Instant (fromDateTime, unInstant, toDateTime, instant)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.Number (fromString) as N
import Data.String as S
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))
import Data.String.Regex as R

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
  case A.uncons $ S.split (S.Pattern "|") data' of
    Just { head: "4", tail } -> tail
    _ -> []

packData' :: String -> String -> String
packData' cmd tag = "5|" <> cmd <> "|" <> tag

unpackData' data' = 
  case A.uncons $ S.split (S.Pattern "|") data' of
    Just { head: "5", tail } -> tail
    _ -> []

millisecondsFromSeconds timeout = Milliseconds (1_000.0 * (toNumber timeout))

maxInArray :: Array Int -> Maybe Int
maxInArray xs =
  case A.uncons xs of
    Nothing -> Nothing
    Just { head, tail } ->
      case maxInArray tail of
        Nothing -> Just head
        Just x -> Just $ max head x

minInArray :: Array Int -> Maybe Int
minInArray xs =
  case A.uncons xs of
    Nothing -> Nothing
    Just { head, tail } ->
      case minInArray tail of
        Nothing -> Just head
        Just x -> Just $ min head x

matchAll r xml =
  let matchLine xml =
        case R.match r xml of
          Nothing -> []
          Just nea ->
            nea 
            # toArray
            # A.mapMaybe identity
  in
  S.split (S.Pattern "\n") xml
  # A.concatMap matchLine
