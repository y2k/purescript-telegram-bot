module Main where

import Prelude

import Affjax (printError)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (Promise, toAffE)
import Data.DateTime (diff)
import Data.Either (Either(..), either)
import Data.Foldable (sequence_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Time.Duration (Minutes(..), Seconds, fromDuration)
import Data.Tuple (Tuple(..))
import Domain as D
import PureDomain as PD
import Effect (Effect)
import Effect.Aff (delay, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)
import PeriodicPostsImages as PI

foreign import unsafeToJson :: ∀ a. a -> Effect String
foreign import unsafeParseJson :: String -> Effect _
foreign import getApiKey :: Effect String

foreign import data Bot :: Type
foreign import createBot :: Effect Bot
foreign import startBotRepl :: Bot -> ({ from :: { id :: Int, first_name :: String }, chat :: Nullable { id :: String, type :: String }, text :: Nullable String, message_id :: Nullable Int, new_chat_member :: Nullable { username :: Nullable String, first_name :: String }, data :: Nullable String, message :: Nullable { message_id :: Int, chat :: { id :: String }, from :: { id :: Int } }, reply_to_message :: Nullable { from :: { id :: Int } } } -> Effect Unit) -> Effect Unit
foreign import sendVideo :: Bot -> { chat_id :: String, reply_to_message_id :: Nullable Int, url :: String, caption :: Nullable String, keyboard :: Array { text :: String, callback_data :: String } } -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: Bot -> { chatId :: String, text :: String, reply_message_id :: Nullable Int } -> Effect (Promise { message_id :: Int })
foreign import deleteMessage :: Bot -> { chatId :: String, messageId :: Int } -> Effect Void
foreign import editMessageMedia :: Bot -> { chat_id :: String, message_id :: Int, url :: String, keyboard :: Array { text :: String, callback_data :: String } } -> Effect Void

foreign import editMessageReplyMarkup :: Bot -> { chat_id :: String, message_id :: Int, keyboard :: Array { text :: String, callback_data :: String } } -> Effect Void

foreign import data Var :: Type -> Type
foreign import makeVar :: ∀ a. a -> Effect (Var a)
foreign import setVar :: ∀ a. Var a -> a -> Effect Unit
foreign import getVar :: ∀ a. Var a -> Effect a

download format url =
  AX.defaultRequest { url = url, method = Left GET, responseFormat = format }
  # AX.request
  >>= (\x -> either (\e -> throw (printError e) # liftEffect) (\x -> pure x.body) x)

editMessageMedia' bot params =
  editMessageMedia bot params
  *> pure unit
  # liftEffect

editMessageReplyMarkup' bot p =
  editMessageReplyMarkup bot p
  *> pure unit
  # liftEffect

deleteMessage' bot p =
  deleteMessage bot { chatId: p.chat, messageId: p.message_id }
  *> pure unit
  # liftEffect

periodicPostsImages bot = do
  start <- liftEffect $ PI.mkStart
  let env =
        { downloadText: \x -> download ResponseFormat.string x.url
        , sendVideo: \x -> (sendVideo bot x # toAffE) }
  let loop = do
        _ <- start env
        delay $ fromDuration $ Minutes 15.0
        loop
  loop

handleMessage bot msg =
  launchAff_ $ do
      apiKey <- liftEffect getApiKey
      nowTime <- liftEffect nowDateTime

      result <- D.update
                  { token: apiKey
                  , delay: delay
                  , downloadJson: download ResponseFormat.json
                  , telegram:
                      { editMessageReplyMarkup: (editMessageReplyMarkup' bot)
                      , editMessageMedia: (editMessageMedia' bot)
                      , sendVideo: (\p -> sendVideo bot p # toAffE)
                      , deleteMessage: (deleteMessage' bot)
                      , sendMessage: (\p -> sendMessage bot p # toAffE) } }
                  msg
                # try
      case result of
        Right _ -> pure unit
        Left e -> log $ "[LOG][ERROR] " <> show e

makeAccessDecorate bot =
  let accessDecorate state next msg = do
        now <- nowDateTime
        currentState <- getVar state

        let duration = diff now currentState.lastResetTime :: Seconds

        let (Tuple effs (Tuple allowNext _)) =
              D.restrictAccess
                { reply : (\text -> do
                    _ <- sendMessage
                            bot
                            { chatId : msg.chat # toMaybe # map (\chat -> chat.id) # fromMaybe ""
                            , text : text
                            , reply_message_id : msg.message_id
                            }
                    pure unit)
                , now
                , updateState : (setVar state) }
                currentState
                msg
        sequence_ effs
        if allowNext
          then next msg
          else pure unit in
  do
    state <- makeVar PD.makeEmptyState
    pure $ accessDecorate state

logDecorate next msg = do
  log $ "[LOG] Message from: " <> msg.from.first_name <> " (" <> (show msg.from.id) <> "), text = " <> (show msg.text)
  next msg

main :: Effect Unit
main = do
  bot <- createBot
  launchAff_ $ periodicPostsImages bot
  accessDecorate <- makeAccessDecorate bot
  startBotRepl bot (logDecorate (accessDecorate (handleMessage bot)))
  log $ "Bot started..."
