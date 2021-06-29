module RerollHandler (handleReroll) where

import Prelude

import Common (chainMessage, packData, unpackData, unwrapEither)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import PureDomain as D

makeRerollVideoRequest info tag message =
  { chat_id: message.chat.id
  , message_id: message.message_id
  , url: (info.data.image_mp4_url :: String)
  , keyboard: [ { callback_data: (packData "reroll" tag), text: "🎲 🎲 🎲" } ] }

mapToModel msg = do
  message <- toMaybe msg.message
  msgInfo <- toMaybe msg.data
  { cmd, cmdArg } <- unpackData msgInfo
  if cmd == "reroll"
    then pure { message, msgInfo, cmdArg }
    else Nothing

handleReroll env msg =
  chainMessage msg mapToModel \{ message, msgInfo, cmdArg } -> do
    json <- D.makeUrl env.token cmdArg # env.downloadJson
    info <- D.parseImageJson json # unwrapEither
    makeRerollVideoRequest info cmdArg message # env.telegram.editMessageMedia
