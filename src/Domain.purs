module Domain (update) where

import Prelude

import Common as C
import Control.Monad.Error.Class (try)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Nullable (null, toMaybe)
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Traversable (sequence)
import Effect.Aff (Aff)
import PureDomain as D

update :: _ -> _ -> Aff Unit
update env msg =
  [ updateHandleCommand
  , updateHandleReroll
  , updateHandleLogin ]
  <#> (\f -> f env msg)
  <#> try # sequence # void

updateHandleCommand env msg = do
  let replyWithVideo = sendVideo env msg
  text <- C.tryExtractCommand msg # C.unwrapMaybe

  case text of
    [ "cat" ] -> replyWithVideo "cat"
    [ "dog" ] -> replyWithVideo "puppy"
    [ "debug_get_user_id" ] -> getUserId env msg
    [ "debug_reply", userId ] -> testMention env msg userId
    _ -> pure unit

updateHandleReroll :: _ -> _ -> Aff Unit
updateHandleReroll env msg = do
  message <- msg.message # C.unwrapNullable
  msgInfo <- msg.data # C.unwrapNullable
  { cmd, cmdArg } <- C.unpackData msgInfo # C.unwrapMaybe
  C.require $ cmd == "reroll"

  json <- D.makeUrl env.token cmdArg # env.downloadJson
  info <- D.parseImageJson json # C.unwrapEither

  D.makeRerollVideoRequest info cmdArg message # env.telegram.editMessageMedia

updateHandleLogin env msg = do
  chat <- msg.chat # C.unwrapNullable
  message_id <- msg.message_id # C.unwrapNullable
  newChatMember <- msg.new_chat_member # C.unwrapNullable

  let username =
        toMaybe newChatMember.username
        # maybe newChatMember.first_name (\username -> "@" <> username)

  json <- D.makeUrl env.token "cat" # env.downloadJson
  info <- D.parseImageJson json # C.unwrapEither

  response <- env.telegram.sendVideo $ D.makeCaptchaRequest chat message_id info username
  _ <- env.delay $ fromDuration $ Seconds $ toNumber D.captchaTimeout
  _ <- env.telegram.deleteMessage { chat_id: chat.id, message_id: response.message_id }
  pure unit

getUserId env msg = do
  chat <- C.unwrapNullable msg.chat
  reply <- C.unwrapNullable msg.reply_to_message
  let userId = reply.from.id :: Int
  response <- env.telegram.sendMessage
                { chatId: chat.id
                , text: "UserID: " <> (show userId)
                , reply_message_id: null }
  _ <- env.delay $ fromDuration $ Seconds 5.0
  _ <- env.telegram.deleteMessage { chat_id: chat.id, message_id: response.message_id }
  pure unit

testMention env msg userId = do
  chat <- C.unwrapNullable msg.chat
  _ <- env.delay $ fromDuration $ Seconds 5.0
  _ <- env.telegram.sendVideo $ D.makeMentionRequest chat userId
  pure unit

sendVideo env msg tag = do
  chat <- C.unwrapNullable msg.chat
  json <- D.makeUrl env.token tag # env.downloadJson
  info <- D.parseImageJson json # C.unwrapEither

  { message_id } <-
    D.sendVideoWithRerollKeyboard chat info tag
    # env.telegram.sendVideo
  _ <- env.delay $ fromDuration $ Seconds 15.0
  env.telegram.editMessageReplyMarkup { chat_id: chat.id, message_id, keyboard: [] }
