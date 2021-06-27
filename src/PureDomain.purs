module PureDomain where

import Prelude

import Common as C
import Data.Argonaut (Json, decodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Map (Map)
import Data.Nullable (notNull, null)
import Data.Time.Duration (Seconds(..))

captchaTimeout = 30
limitCount = 2
limitPerSedonds = Seconds 15.0

type State = { lastResetTime ∷ DateTime , users ∷ Map Int Int }

makeEmptyState :: State
makeEmptyState = { lastResetTime : bottom, users : mempty }

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

makeMentionRequest chat userId =
  { chat_id: chat.id
  , reply_to_message_id: null
  , url: "https://i.giphy.com/media/fT3PPZwB2lZMk/giphy.gif"
  , caption: notNull $ "Привет [user_name](tg://user?id=" <> userId <> "), оптишись плиз, была ли нотификация"
  , keyboard: [] }

makeCaptchaRequest chat message_id info username =
  { chat_id: chat.id
  , reply_to_message_id: notNull message_id
  , url: info.data.image_mp4_url
  , caption: username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show captchaTimeout) <> " секунд 😸" # notNull
  , keyboard: [] }
