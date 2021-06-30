module Main where

import Prelude

import AccessDecorator (makeAccessDecorate)
import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Common (bindBotPart, logFunctionDecorate, unwrapMaybe)
import Control.Promise (toAffE)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import HandleMessageDecorator (makeHanleMessageUpdate)
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

-- makeEnv bot apiKey =
--   { token: apiKey
--   , delay: delay
--   , downloadJson: download json
--   , telegram:
--       { editMessageReplyMarkup: logFunctionDecorate "editMessageReplyMarkup" (\p -> (editMessageReplyMarkup bot) p *> pure unit # liftEffect)
--       , editMessageMedia: logFunctionDecorate "editMessageMedia" (\p -> (editMessageMedia bot) p *> pure unit # liftEffect)
--       , sendVideo: logFunctionDecorate "sendVideo" (\p -> (sendVideo bot) p # toAffE)
--       , deleteMessage: logFunctionDecorate "deleteMessage" (\p -> (deleteMessage bot) p *> pure unit # liftEffect)
--       , sendMessage: logFunctionDecorate "sendMessage" (\p -> (sendMessage bot) p # toAffE)
--       }
--   }

-- makeHandleMessageDecorator sendMessage deleteMessage sendVideo editMessageMedia editMessageReplyMarkup download delay apiKey nowDateTime = do
makeHandleBotMessage env = do
  nowTime <- liftEffect nowDateTime
  -- pure $ router env
  -- pure makeHanleMessageUpdate
  -- pure $ handleLogin env
  -- pure $ (bindBotPart
  --     (handleReroll env)
  --     (handleLogin env))
  pure $
    bindBotPart
      logDecorate
      (bindBotPart
        (bindBotPart
          (handleReroll env)
          (handleLogin env))
        (handlePostGif env))

-- main :: Effect Unit
-- main :: forall m. MonadEffect m => m Unit
main :: Aff Unit
main = do
  apiKey <- lookupEnv "GIPHY_API_KEY" # liftEffect >>= unwrapMaybe
  bot <- liftEffect createBot
  -- let env = makeEnv bot apiKey
  let env = { token: apiKey
            , delay: delay
            , downloadJson: download json
            , telegram:
                { editMessageReplyMarkup: logFunctionDecorate "editMessageReplyMarkup" (\p -> (editMessageReplyMarkup bot) p # toAffE)
                , editMessageMedia: logFunctionDecorate "editMessageMedia" (\p -> (editMessageMedia bot) p # toAffE)
                , sendVideo: logFunctionDecorate "sendVideo" (\p -> (sendVideo bot) p # toAffE)
                , deleteMessage: logFunctionDecorate "deleteMessage" (\p -> (deleteMessage bot) p # toAffE)
                , sendMessage: logFunctionDecorate "sendMessage" (\p -> (sendMessage bot) p # toAffE)
                }
            }
  -- let handleMessageDecorator = makeHandleMessageDecorator (sendMessage bot) (deleteMessage bot) (sendVideo bot) (editMessageMedia bot) (editMessageReplyMarkup bot) download delay apiKey nowDateTime
  -- let handleBotMessage = makeHandleBotMessage bot
  -- launchAff_ $ runPeriodicPostsImages (sendVideo bot) download delay
  -- accessDecorate <- makeAccessDecorate (sendMessage bot)
  -- startBotRepl bot (logDecorate (accessDecorate handleMessageDecorator))
  accessDecorate <- makeAccessDecorate (sendMessage bot)

  handleBotMessage <- makeHandleBotMessage env
  -- ?TODO handleBotMessage

  -- let _ = ?TODO handleBotMessage

  liftEffect $ startBotRepl bot (\msg -> handleBotMessage msg # launchAff_)
  log $ "Bot started..."
