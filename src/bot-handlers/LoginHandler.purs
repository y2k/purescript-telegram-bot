module LoginHandler (handleLogin) where

import Prelude

import Common (BotMessage, chainMessage, unwrapEither, unwrapMaybe)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Nullable (notNull, toMaybe)
import Data.Time.Duration (Seconds(..), fromDuration)
import PureDomain as D

makeCaptchaRequest chat message_id info username =
  { chat_id: chat.id
  , reply_to_message_id: notNull message_id
  , url: info.data.image_mp4_url
  , caption: username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show D.captchaTimeout) <> " секунд 😸" # notNull
  , keyboard: [] }

extractModel (msg :: BotMessage) = do
  chat <- toMaybe msg.chat
  message_id <- toMaybe msg.message_id
  newChatMember <- toMaybe msg.new_chat_member
  let username = maybe newChatMember.first_name ((<>) "@") (toMaybe newChatMember.username)
  pure { chat, message_id, newChatMember, username }

handleLogin' env msg =
  chainMessage msg extractModel \{ chat, message_id, newChatMember, username } -> do
    json <- D.makeUrl env.token "cat" # env.downloadJson
    info <- D.parseImageJson json # unwrapEither
    telMessage <- env.telegram.sendVideo $ makeCaptchaRequest chat message_id info username
    _ <- env.delay $ fromDuration $ Seconds $ toNumber D.captchaTimeout
    env.telegram.deleteMessage { chat_id: chat.id, message_id: telMessage.message_id }

handleLogin env msg = do
  { chat, message_id, newChatMember, username } <- extractModel msg # unwrapMaybe

  json <- D.makeUrl env.token "cat" # env.downloadJson
  info <- D.parseImageJson json # unwrapEither
  response <- env.telegram.sendVideo $ makeCaptchaRequest chat message_id info username
  _ <- env.delay $ fromDuration $ Seconds $ toNumber D.captchaTimeout
  _ <- env.telegram.deleteMessage { chat_id: chat.id, message_id: response.message_id }
  pure unit
