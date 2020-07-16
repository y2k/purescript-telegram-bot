module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (Promise, toAffE)
import Data.Array (concat)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Nullable (Nullable, toNullable)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Domain (Cmd(..), update)
import Effect.Now (nowDateTime)

foreign import data Bot :: Type
foreign import getApiKey :: Effect String
foreign import editMessageMedia :: Bot -> Int -> Int -> String -> Array { text :: String, callback_data :: String } -> Effect Void
foreign import sendVideo :: Bot -> Int -> Nullable Int -> String-> Nullable String -> Array { text :: String, callback_data :: String } -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: Bot -> Int -> String -> Effect Void
foreign import deleteMessage :: Bot -> Int -> Int -> Effect Void
foreign import startBotRepl :: ({ from :: { id :: Int }, bot :: Bot, chat :: Nullable { id :: Int }, text :: String, id :: Int, regUserName :: Nullable String, data :: Nullable String, message :: Nullable { message_id :: Int, chat :: { id :: Int }, from :: { id :: Int } } } -> Effect Unit) -> Effect Unit

executeCmd :: Bot -> Cmd -> Aff (Array Cmd)
executeCmd bot cmd =
  case cmd of
    EditVideo x -> 
      editMessageMedia bot x.chatId x.messageId x.url x.keyboard
      *> pure []
      # liftEffect
    SendVideo chat msgId url caption keyboard f -> do
      msg <- toAffE $ sendVideo bot chat (toNullable msgId) url (toNullable caption) keyboard
      pure $ f msg.message_id
    SendMessage chat text -> sendMessage bot chat text *> pure [] # liftEffect
    Delay duration nextCmds -> do
      delay duration
      pure nextCmds
    DeleteMessage chat msgId -> deleteMessage bot chat msgId *> pure [] # liftEffect
    DownloadJson url f -> do
      result <-
        AX.defaultRequest { url = url, method = Left GET, responseFormat = ResponseFormat.json }
        # AX.request
      pure $ f (map (\r -> { body : r.body }) result)

executeCmds :: Bot -> Array Cmd -> Aff Unit
executeCmds bot cmds =
  case cmds of
    [] -> pure unit
    _ -> do
      let effects = cmds # map (executeCmd bot)
      newCmds <- sequence effects
      executeCmds bot (concat newCmds)

main :: Effect Unit
main = do
  startBotRepl (\msg -> launchAff_ $ do
    apiKey <- liftEffect getApiKey
    nowTime <- liftEffect nowDateTime
    let cmds = update { apiKey : apiKey, now : nowTime } msg
    executeCmds msg.bot cmds)
  log $ "Bot started..."
