module PostGifHandler (handlePostGif) where

import Prelude

import Common (packData, tryExtractCommand, unwrapEither, unwrapMaybe, unwrapNullable)
import Data.Nullable (null)
import Data.Time.Duration (Seconds(..), fromDuration)
import PureDomain as D

sendVideoWithRerollKeyboard chat info tag =
  { chat_id: chat.id
  , reply_to_message_id: null
  , url: info.data.image_mp4_url
  , caption: null
  , keyboard: [ { callback_data: (packData "reroll" tag), text: "🎲 🎲 🎲" } ] }

handlePostGif env msg = do
  let replyWithVideo = sendVideo env msg
  text <- tryExtractCommand msg # unwrapMaybe
  case text of
    [ "cat" ] -> replyWithVideo "cat"
    [ "dog" ] -> replyWithVideo "puppy"
    _ -> pure unit

sendVideo env msg tag = do
  chat <- unwrapNullable msg.chat
  json <- D.makeUrl env.token tag # env.downloadJson
  info <- D.parseImageJson json # unwrapEither

  { message_id } <-
    sendVideoWithRerollKeyboard chat info tag
    # env.telegram.sendVideo
  _ <- env.delay $ fromDuration $ Seconds 15.0
  env.telegram.editMessageReplyMarkup { chat_id: chat.id, message_id, keyboard: [] }
