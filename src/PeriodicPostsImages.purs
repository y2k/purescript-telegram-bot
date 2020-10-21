module PeriodicPostsImages (start, handleMessage, State, emptyState, mkStart) where

import Prelude

import Common as C
import Data.Array as A
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import Data.Either (Either(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.Regex.Flags as RF
import Data.String.Regex.Unsafe as RU
import Effect (Effect)
import Effect.Aff (Aff, catchError, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (new, read, write)

type State = { loaded :: Maybe Int, chats :: Set String }

emptyState = { loaded: Nothing, chats: Set.empty }

handleMessage :: State -> String -> State
handleMessage state _ = state

mkStart :: Effect (_ -> Aff Unit)
mkStart = do
  state <- new emptyState
  pure (\env -> do
    s <- liftEffect $ read state
    x <- start env s
    liftEffect $ write x state)

start :: _ -> State -> Aff State
start env state = do
  xml <-
    catchError
      (env.downloadText { url: "http://joyreactor.cc/rss/tag/%D0%BB%D0%B8%D1%87%D0%B8%D0%BD%D0%BA%D0%B0%2B%D0%BA%D0%BE%D1%82%D1%8D" })
      (\_ -> do
        log "[ERROR] Network error while load RSS"
        pure "")
  let ids = parseGifIds xml
  case state.loaded of
    Nothing ->
      case C.maxInArray ids of
        Just id -> pure $ state { loaded = Just id }
        Nothing -> pure state
    Just lastSendedId -> do
      case getNewId lastSendedId ids of
        Just newId -> do
          let urls = makeVideoUrl newId
          result <- try $ env.sendVideo { chat: "-1001130908027", url: urls.video, caption: notNull "#котэ вам в ленту" }
          case result of
            Left err -> do
              _ <- env.sendVideo { chat: "-1001130908027", url: urls.gif, caption: notNull "#котэ вам в ленту" }
              pure $ state { loaded = Just newId }
            Right resp ->
              pure $ state { loaded = Just newId }
        Nothing -> pure state

getNewId :: Int -> Array Int -> Maybe Int
getNewId x xs = A.filter (_ > x) xs # C.minInArray

makeVideoUrl :: Int -> { gif :: String, video :: String }
makeVideoUrl id =
  { video: "http://img0.joyreactor.cc/pics/post/mp4/-" <> (show id) <> ".mp4",
    gif: "http://img0.joyreactor.cc/pics/post/-" <> (show id) <> ".gif" }

parseGifIds :: String -> Array Int
parseGifIds xml = C.matchAll videoRegex xml # A.mapMaybe fromString

videoRegex = RU.unsafeRegex """"url": "http://img\d\.joyreactor\.cc/pics/post/full/.+?-(\d+).gif"""" RF.noFlags
