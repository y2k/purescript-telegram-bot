module TelegramApi where

import Prelude

import Control.Promise (Promise)
import Data.Nullable (Nullable)
import Effect (Effect)

foreign import data Bot :: Type
foreign import createBot :: Effect Bot
foreign import startBotRepl :: Bot -> ({ from :: { id :: Int, first_name :: String }, chat :: Nullable { id :: String, type :: String }, text :: Nullable String, message_id :: Nullable Int, new_chat_member :: Nullable { username :: Nullable String, first_name :: String }, data :: Nullable String, message :: Nullable { message_id :: Int, chat :: { id :: String }, from :: { id :: Int } }, reply_to_message :: Nullable { from :: { id :: Int } } } -> Effect Unit) -> Effect Unit
foreign import sendVideo :: Bot -> { chat_id :: String, reply_to_message_id :: Nullable Int, url :: String, caption :: Nullable String, keyboard :: Array { text :: String, callback_data :: String } } -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: Bot -> { chatId :: String, text :: String, reply_message_id :: Nullable Int } -> Effect (Promise { message_id :: Int })
foreign import deleteMessage :: Bot -> { chat_id :: String, message_id :: Int } -> Effect Void
foreign import editMessageMedia :: Bot -> { chat_id :: String, message_id :: Int, url :: String, keyboard :: Array { text :: String, callback_data :: String } } -> Effect Void
foreign import editMessageReplyMarkup :: Bot -> { chat_id :: String, message_id :: Int, keyboard :: Array { text :: String, callback_data :: String } } -> Effect Void
