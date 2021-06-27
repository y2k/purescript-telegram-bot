module RerollHandler (handleReroll) where

import Prelude

import Common (packData, require, unpackData, unwrapEither, unwrapMaybe, unwrapNullable)
import PureDomain as D

makeRerollVideoRequest info tag message =
  { chat_id: message.chat.id
  , message_id: message.message_id
  , url: (info.data.image_mp4_url :: String)
  , keyboard: [ { callback_data: (packData "reroll" tag), text: "🎲 🎲 🎲" } ] }

handleReroll env msg = do
  message <- msg.message # unwrapNullable
  msgInfo <- msg.data # unwrapNullable
  { cmd, cmdArg } <- unpackData msgInfo # unwrapMaybe
  require $ cmd == "reroll"

  json <- D.makeUrl env.token cmdArg # env.downloadJson
  info <- D.parseImageJson json # unwrapEither

  makeRerollVideoRequest info cmdArg message # env.telegram.editMessageMedia
