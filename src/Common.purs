module Common where

import Prelude

import Data.Array (concat, drop)
import Data.Array as A
import Data.Array.NonEmpty (index, toArray)
import Data.DateTime (diff)
import Data.DateTime.Instant (fromDateTime, unInstant, toDateTime, instant)
import Data.Either (Either(..))
import Data.Foldable (indexl)
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Data.Number (fromString) as N
import Data.String (Pattern(..), split)
import Data.String as S
import Data.String.Regex (match)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error)
import Effect.Exception (throw)
import Effect.Ref (Ref)
import Effect.Ref as Ref

newRef ∷ ∀ m a. MonadEffect m ⇒ a → m (Ref a)
newRef = liftEffect <<< Ref.new

readRef ∷ ∀ m a. MonadEffect m ⇒ Ref a → m a
readRef = liftEffect <<< Ref.read

writeRef ∷ ∀ m a. MonadEffect m ⇒ Ref a → a → m Unit
writeRef r = liftEffect <<< flip Ref.write r

modifyRef ∷ ∀ m a. MonadEffect m ⇒ Ref a → (a → a) → m a
modifyRef r = liftEffect <<< flip Ref.modify r

unwrapNullable m = toMaybe m # unwrapMaybe

unwrapMaybe m =
  case m of
    Just x -> pure x
    Nothing -> throw "no value" # liftEffect

unwrapEither m =
  case m of
    Right x -> pure x
    Left e -> throw e # liftEffect

toIntOrZero x = fromString x # maybe 0 identity

tryExtractCommand msg = do
  text <- toMaybe msg.text
  let ps = split (Pattern " ") text # drop 1
  pairs <- match (unsafeRegex "/([^@ ]+).*?" noFlags) text
  optCmd <- index pairs 1
  cmd <- optCmd
  pure $ concat [ [cmd], ps ]

serializeDateTime d = d # fromDateTime # unInstant # unwrap # show

deserializeDateTime str = N.fromString str <#> Milliseconds >>= instant <#> toDateTime

timeInRange to from maxDiff = diff to from < maxDiff

require cond =
  if cond then pure unit else error "asset failed"

packData :: String -> String -> String
packData cmd tag = "6|" <> cmd <> "|" <> tag

unpackData text = do
  m <- match (unsafeRegex "6\\|(.+?)\\|(.+)" noFlags) text
  cmd <- indexl 1 m >>= identity
  cmdArg <- indexl 2 m >>= identity
  pure { cmd, cmdArg }

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
