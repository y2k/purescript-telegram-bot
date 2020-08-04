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
    Nothing -> ?TODO
    Just lastSendedId -> do
      case getNewId lastSendedId ids of
        Just newId -> do
          cmd <- env.sendVideo "" (makeVideoUrl newId)
          pure { newState: state { loaded: newId }, cmd: [ cmd ] }
        Nothing -> ?TODO

getNewId :: Int -> Array Int -> Maybe Int
getNewId _ _ = ?TODO

makeVideoUrl :: Int -> String
makeVideoUrl id = ?TODO

parseGifIds :: String -> Array Int
parseGifIds xml = ?TODO
