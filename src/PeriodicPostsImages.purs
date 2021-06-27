module PeriodicPostsImages (start, handleMessage, State, emptyState, mkStart, runPeriodicPostsImages) where

import Prelude

import Affjax.ResponseFormat (string)
import Common as C
import Control.Promise (toAffE)
import Data.Array as A
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Regex.Flags as RF
import Data.String.Regex.Unsafe as RU
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect (Effect)
import Effect.Aff (Aff, catchError)
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
          let sendCatVideo url =
                env.sendVideo
                  { chat_id: "-1001130908027"
                  , url: url
                  , caption: notNull "время постить #котиков #котик"
                  , reply_to_message_id: null
                  , keyboard: [] }
          _ <- catchError (sendCatVideo urls.video) (\_ -> sendCatVideo urls.gif)
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

runPeriodicPostsImages :: _ -> _ -> _ -> Aff Unit
runPeriodicPostsImages sendVideo download delay = do
  start <- liftEffect $ mkStart
  let env =
        { downloadText: \x -> download string x.url
        , sendVideo: \x -> (sendVideo x # toAffE) }
  let loop = do
        _ <- start env
        _ <- delay $ fromDuration $ Minutes 15.0
        loop
  loop

videoRegex = RU.unsafeRegex """"url": "http://img\d\.joyreactor\.cc/pics/post/full/.+?-(\d+).gif"""" RF.noFlags
