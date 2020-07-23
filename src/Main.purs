module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (Promise, toAffE)
import Data.Array (concat)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Nullable (Nullable, null, toNullable)
import Data.Traversable (sequence)
import Domain (Cmd(..))
import Domain2 as D
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Now (nowDateTime)

foreign import data Bot :: Type
foreign import unsafeToJson :: forall a. a -> Effect String
foreign import unsafeParseJson :: String -> Effect _
foreign import getApiKey :: Effect String
foreign import editMessageReplyMarkup :: Bot -> Int -> Int -> Array { text :: String, callback_data :: String } -> Effect Void
foreign import editMessageMedia :: Bot -> Int -> Int -> String -> Array { text :: String, callback_data :: String } -> Effect Void
foreign import sendVideo :: Bot -> Int -> Nullable Int -> String-> Nullable String -> Array { text :: String, callback_data :: String } -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: { chatId :: Int, text :: String } -> Bot -> Effect Void
foreign import deleteMessage :: Bot -> { chatId :: Int, messageId :: Int } -> Effect Void
foreign import startBotRepl :: ({ from :: { id :: Int }, bot :: Bot, chat :: Nullable { id :: Int }, text :: Nullable String, message_id :: Nullable Int, new_chat_member :: Nullable { username :: Nullable String }, data :: Nullable String, message :: Nullable { message_id :: Int, chat :: { id :: Int }, from :: { id :: Int } } } -> Effect Unit) -> Effect Unit

executeCmd :: Bot -> Cmd -> Aff (Array Cmd)
executeCmd bot cmd =
  case cmd of
    EditMessageButtons x -> editMessageReplyMarkup bot x.chatId x.messageId x.keyboard *> pure [] # liftEffect
    EditVideo x -> editMessageMedia bot x.chatId x.messageId x.url x.keyboard *> pure [] # liftEffect
    SendVideo x -> do
      msg <- toAffE $ sendVideo bot x.chat (toNullable x.msgId) x.url (toNullable x.caption) x.keyboard
      pure $ x.f msg.message_id
    SendMessage x -> sendMessage x bot *> pure [] # liftEffect
    Delay x -> do
      delay x.duration
      pure x.commands
    DeleteMessage x -> deleteMessage bot x *> pure [] # liftEffect
    DownloadJson x -> do
      result <-
        AX.defaultRequest { url = x.url, method = Left GET, responseFormat = ResponseFormat.json }
        # AX.request
      pure $ x.f (map (\r -> { body : r.body }) result)

executeCmds :: Bot -> Array Cmd -> Aff Unit
executeCmds bot cmds =
  case cmds of
    [] -> pure unit
    _ -> do
      let effects = cmds # map (executeCmd bot)
      newCmds <- sequence effects
      executeCmds bot (concat newCmds)

downloadJson url =
  let r = AX.defaultRequest { url = url, method = Left GET, responseFormat = ResponseFormat.json }
          # AX.request
  in
  bind r (\x -> either (\e -> throw "" # liftEffect) (\x -> pure x.body) x)

sendVideo' bot chat msgId url caption keyboard = do
  msg <- toAffE $ sendVideo bot chat (toNullable msgId) url (toNullable caption) keyboard
  pure msg.message_id

sendVideo'' bot param = do
  msg <- toAffE $ sendVideo bot param.chat null param.url param.caption param.keyboard
  pure msg.message_id

-- foreign import editMessageMedia :: Bot -> Int -> Int -> String -> Array { text :: String, callback_data :: String } -> Effect Void
editVideo' bot chat msgId url keyboard =
  editMessageMedia bot chat msgId url keyboard
  *> pure unit # liftEffect

updateKeyboard' :: _ -> _ -> _ -> _ -> Aff _
updateKeyboard' bot chatId messageId keyboard =
  editMessageReplyMarkup bot chatId messageId keyboard *> pure unit # liftEffect

main :: Effect Unit
main = do
  startBotRepl (\msg -> launchAff_ $ do
    apiKey <- liftEffect getApiKey
    nowTime <- liftEffect nowDateTime

-- env.telegram.sendVideo2 
--   { chat: chat.id
--   , url: info.data.image_mp4_url 
--   , caption: Just caption
--   , keyboard: [] }

    _ <- D.update
           { token: apiKey
           , delay: delay
           , downloadJson: downloadJson
           , telegram:
               { updateKeyboard: (updateKeyboard' msg.bot)
               , editVideo: (editVideo' msg.bot)
               , sendVideo: (sendVideo'' msg.bot) } }
           msg

    -- let cmds = update { apiKey : apiKey, now : nowTime } msg
    -- executeCmds msg.bot cmds
    pure unit)
  log $ "Bot started..."
