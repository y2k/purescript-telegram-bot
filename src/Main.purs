module Main where

import Prelude

import AccessDecorator (makeAccessDecorate)
import Affjax (printError)
import Affjax as AX
import Common (unwrapMaybe)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import HandleMessageDecorator (makeHandleMessageDecorator)
import LogDecorator (logDecorate)
import Node.Process (lookupEnv)
import PeriodicPostsImages (runPeriodicPostsImages)
import TelegramApi (createBot, deleteMessage, editMessageMedia, editMessageReplyMarkup, sendMessage, sendVideo, startBotRepl)

download format url =
  AX.defaultRequest { url = url, method = Left GET, responseFormat = format }
  # AX.request
  >>= (\x -> either (\e -> throw (printError e) # liftEffect) (\x -> pure x.body) x)

main :: Effect Unit
main = do
  apiKey <- lookupEnv "GIPHY_API_KEY" >>= unwrapMaybe
  bot <- createBot
  let handleMessageDecorator = makeHandleMessageDecorator (sendMessage bot) (deleteMessage bot) (sendVideo bot) (editMessageMedia bot) (editMessageReplyMarkup bot) download delay apiKey nowDateTime
  launchAff_ $ runPeriodicPostsImages (sendVideo bot) download delay
  accessDecorate <- makeAccessDecorate (sendMessage bot)
  startBotRepl bot (logDecorate (accessDecorate handleMessageDecorator))
  log $ "Bot started..."
