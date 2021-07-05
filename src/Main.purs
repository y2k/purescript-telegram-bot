module Main where

import Prelude

import AccessDecorator (makeAccessDecorate)
import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Common (bindBotPart, logFunctionDecorate, unwrapMaybe)
import Control.Promise (toAffE)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import LogDecorator (logDecorate)
import LoginHandler (handleLogin)
import Node.Process (lookupEnv)
import PeriodicPostsImages (runPeriodicPostsImages)
import PostGifHandler (handlePostGif)
import RerollHandler (handleReroll)
import TelegramApi (createBot, deleteMessage, editMessageMedia, editMessageReplyMarkup, sendMessage, sendVideo, startBotRepl)

download format url =
  AX.defaultRequest { url = url, method = Left GET, responseFormat = format }
  # AX.request
  >>= (\x -> either (\e -> throw (printError e) # liftEffect) (\x -> pure x.body) x)

emptyDecorator msg = pure $ Just msg

handleUpdate accessDecorate env =
  [ logDecorate
  , accessDecorate
  , (handlePostGif env)
  , (handleReroll env)
  , (handleLogin env)
  ] # foldl bindBotPart emptyDecorator

mainAsync :: Aff _
mainAsync = do
  apiKey <- lookupEnv "GIPHY_API_KEY" # liftEffect >>= unwrapMaybe
  bot <- liftEffect createBot
  let env = { token: apiKey
            , delay: delay
            , downloadJson: download json
            , telegram:
                { editMessageReplyMarkup: logFunctionDecorate "editMessageReplyMarkup" (editMessageReplyMarkup bot >>> toAffE)
                , editMessageMedia: logFunctionDecorate "editMessageMedia" (editMessageMedia bot >>> toAffE)
                , sendVideo: logFunctionDecorate "sendVideo" (sendVideo bot >>> toAffE)
                , deleteMessage: logFunctionDecorate "deleteMessage" (deleteMessage bot >>> toAffE)
                , sendMessage: logFunctionDecorate "sendMessage" (sendMessage bot >>> toAffE)
                }
            }

  accessDecorate <- makeAccessDecorate (sendMessage bot >>> liftEffect)
  let handleBotMessage = handleUpdate accessDecorate env

  log $ "Bot started..."

  [ runPeriodicPostsImages env.telegram.sendVideo download delay
  , liftEffect $ startBotRepl bot (handleBotMessage >>> launchAff_)
  ] # traverse forkAff

main :: Effect Unit
main = launchAff_ mainAsync
