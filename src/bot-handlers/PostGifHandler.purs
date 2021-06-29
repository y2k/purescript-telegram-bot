module PostGifHandler (handlePostGif) where

import Prelude

import Common (chainMessage, packData, tryExtractCommand, unwrapEither, unwrapMaybe)
import Data.Maybe (Maybe(..))
import Data.Nullable (null, toMaybe)
import Data.Time.Duration (Seconds(..), fromDuration)
import PureDomain as D

sendVideoWithRerollKeyboard chat info tag =
  { chat_id: chat.id
  , reply_to_message_id: null
  , url: info.data.image_mp4_url
  , caption: null
  , keyboard: [ { callback_data: (packData "reroll" tag), text: "🎲 🎲 🎲" } ] }

mapToTag msg = do
  text <- tryExtractCommand msg
  chat <- toMaybe msg.chat
  tag <-
    case text of
      [ "cat" ] -> Just "cat"
      [ "dog" ] -> Just "puppy"
      _ -> Nothing
  pure { chat, tag }

handlePostGif' env msg =
  chainMessage msg mapToTag \{ chat, tag } -> do
    json <- D.makeUrl env.token tag # env.downloadJson
    info <- D.parseImageJson json # unwrapEither
    { message_id } <-
      sendVideoWithRerollKeyboard chat info tag
      # env.telegram.sendVideo
    _ <- env.delay $ fromDuration $ Seconds 15.0
    env.telegram.editMessageReplyMarkup { chat_id: chat.id, message_id, keyboard: [] }

handlePostGif env msg = do
  { chat, tag } <- mapToTag msg # unwrapMaybe

  json <- D.makeUrl env.token tag # env.downloadJson
  info <- D.parseImageJson json # unwrapEither
  { message_id } <-
    sendVideoWithRerollKeyboard chat info tag
    # env.telegram.sendVideo
  _ <- env.delay $ fromDuration $ Seconds 15.0
  env.telegram.editMessageReplyMarkup { chat_id: chat.id, message_id, keyboard: [] }
