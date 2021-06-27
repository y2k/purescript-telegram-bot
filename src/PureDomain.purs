module PureDomain where

import Prelude

import Common as C
import Data.Argonaut (Json, decodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Map (Map)
import Data.Nullable (null)
import Data.Time.Duration (Seconds(..))

type State = { lastResetTime ∷ DateTime , users ∷ Map Int Int }

makeEmptyState :: State
makeEmptyState = { lastResetTime : bottom, users : mempty }

limitCount = 2
limitPerSedonds = Seconds 15.0

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

makeUrl token tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> token <> "&tag=" <> tag

sendVideoWithRerollKeyboard chat info tag =
  { chat_id: chat.id
  , reply_to_message_id: null
  , url: info.data.image_mp4_url
  , caption: null
  , keyboard: [ { callback_data: (C.packData "reroll" tag), text: "🎲 🎲 🎲" } ] }

makeRerollVideoRequest info tag message =
  { chat_id: message.chat.id
  , message_id: message.message_id
  , url: (info.data.image_mp4_url :: String)
  , keyboard: [ { callback_data: (C.packData "reroll" tag), text: "🎲 🎲 🎲" } ] }
