module TelegramApi where

import Prelude

import Common (BotMessage)
import Control.Promise (Promise)
import Data.Nullable (Nullable)
import Effect (Effect)

foreign import data Bot :: Type
foreign import createBot :: Effect Bot
foreign import startBotRepl :: Bot -> (BotMessage -> Effect Unit) -> Effect Unit
foreign import sendVideo :: Bot -> { chat_id :: String, reply_to_message_id :: Nullable Int, url :: String, caption :: Nullable String, keyboard :: Array { text :: String, callback_data :: String } } -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: Bot -> { chatId :: String, text :: String, reply_message_id :: Nullable Int } -> Effect (Promise { message_id :: Int })
foreign import deleteMessage :: Bot -> { chat_id :: String, message_id :: Int } -> Effect (Promise Void)
foreign import editMessageMedia :: Bot -> { chat_id :: String, message_id :: Int, url :: String, keyboard :: Array { text :: String, callback_data :: String } } -> Effect (Promise Void)
foreign import editMessageReplyMarkup :: Bot -> { chat_id :: String, message_id :: Int, keyboard :: Array { text :: String, callback_data :: String } } -> Effect (Promise Void)
