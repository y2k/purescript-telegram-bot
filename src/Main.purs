module Main where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Nullable (Nullable, null)
import Data.Time.Duration (Minutes(..), fromDuration)
import Domain as D
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import PeriodicPostsImages as PI

foreign import data Bot :: Type
foreign import unsafeToJson :: forall a. a -> Effect String
foreign import unsafeParseJson :: String -> Effect _
foreign import getApiKey :: Effect String
foreign import editMessageReplyMarkup :: Bot -> String -> Int -> Array { text :: String, callback_data :: String } -> Effect Void
foreign import editMessageMedia :: Bot -> String -> Int -> String -> Array { text :: String, callback_data :: String } -> Effect Void
foreign import sendVideo :: Bot -> String -> Nullable Int -> String -> Nullable String -> Array { text :: String, callback_data :: String } -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: { chatId :: String, text :: String } -> Bot -> Effect (Promise Void)
foreign import deleteMessage :: Bot -> { chatId :: String, messageId :: Int } -> Effect Void
foreign import createBot :: Effect Bot
foreign import startBotRepl :: Bot -> ({ from :: { id :: Int, first_name :: String }, chat :: Nullable { id :: String }, text :: Nullable String, message_id :: Nullable Int, new_chat_member :: Nullable { username :: Nullable String, first_name :: String }, data :: Nullable String, message :: Nullable { message_id :: Int, chat :: { id :: String }, from :: { id :: Int } } } -> Effect Unit) -> Effect Unit

download format url =
  let r = AX.defaultRequest { url = url, method = Left GET, responseFormat = format } # AX.request in
  bind r (\x -> either (\e -> throw (printError e) # liftEffect) (\x -> pure x.body) x)

downloadJson = download ResponseFormat.json
downloadText = download ResponseFormat.string

sendVideo' bot param = do
  msg <- toAffE $ sendVideo bot param.chat param.reply_message_id param.url param.caption param.keyboard
  pure msg.message_id

editVideo' bot p =
  editMessageMedia bot p.chat p.messageId p.url p.keyboard
  *> pure unit # liftEffect

updateKeyboard' bot p =
  editMessageReplyMarkup bot p.chat p.messageId p.keyboard *> pure unit # liftEffect

deleteMessage' bot p =
  deleteMessage bot { chatId: p.chat, messageId: p.message_id } *> pure unit # liftEffect

periodicPostsImages bot = do
  start <- liftEffect $ PI.mkStart
  let env =
        { downloadText: \x -> downloadText x.url
        , sendVideo: \x -> (sendVideo' bot ({ chat: x.chat, url: x.url, caption: x.caption, reply_message_id: null, keyboard: [] })) }
  let loop = do
        _ <- start env
        delay $ fromDuration $ Minutes 15.0
        loop
  loop

main :: Effect Unit
main = do
  bot <- createBot
  launchAff_ $ periodicPostsImages bot
  startBotRepl bot (\msg -> launchAff_ $ do
    apiKey <- liftEffect getApiKey
    nowTime <- liftEffect nowDateTime

    log $ "[LOG] Message from: " <> msg.from.first_name <> " (" <> (show msg.from.id) <> "), text = " <> (show msg.text)

    D.update
      { token: apiKey
      , delay: delay
      , downloadJson: downloadJson
      , telegram:
          { updateKeyboard: (updateKeyboard' bot)
          , editVideo: (editVideo' bot)
          , sendVideo: (sendVideo' bot)
          , deleteMessage: (deleteMessage' bot) } }
      msg)
  log $ "Bot started..."
