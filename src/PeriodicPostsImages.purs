module PeriodicPostsImages (start, handleMessage, State) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff (Aff)

--
--

test :: Aff Unit
test = do
  let env = { downloadText: (\_ -> pure ""), sendVideo: (\_ _ -> pure 0) }
  _ <- start env emptyState
  pure unit

--
--

type State = { loaded :: Maybe Int, chats :: Set String }

emptyState = { loaded: Nothing, chats: Set.empty }

handleMessage :: State -> String -> State
handleMessage state _ = state

start :: _ -> State -> Aff { newState :: State, cmd :: _ }
start env state = do
  xml <- env.downloadText { url: "http://joyreactor.cc/rss/tag/личинка+котэ" }
  let ids = parseGifIds xml
  case state.loaded of
    Nothing ->
      case minId ids of
        Just id -> pure { newState: state { loaded = Just id }, cmd: [] }
        Nothing -> pure { newState: state, cmd: [] }
    Just lastSendedId -> do
      case getNewId lastSendedId ids of
        Just newId -> do
          cmd <- env.sendVideo "" (makeVideoUrl newId)
          pure { newState: state { loaded = Just newId }, cmd: [ cmd ] }
        Nothing -> pure { newState: state, cmd: [] }

minId :: Array Int -> Maybe Int
minId _ = Nothing -- FIXME:

getNewId :: Int -> Array Int -> Maybe Int
getNewId _ _ = Nothing -- FIXME:

makeVideoUrl :: Int -> String
makeVideoUrl id = "" -- FIXME:

parseGifIds :: String -> Array Int
parseGifIds xml = [] -- FIXME:
