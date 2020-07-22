module Domain2 (update) where

import Prelude

import Data.Argonaut (Json, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)

update :: _ -> _ -> Aff Unit
update env msg =
  case toMaybe msg.text of
    Just "/cat" -> sendVideo env msg "cat"
    Just "/dog" -> sendVideo env msg "puppy"
    _ -> pure unit

sendVideo env msg tag =
  case toMaybe msg.chat of
    Just chat -> do
      let url = makeUrl env.token tag
      json <- env.downloadJson url
      case parseImageJson json of
        Right info -> do
          id <- env.telegram.sendVideo chat.id info.data.image_mp4_url [ { callback_data: "1", text: "🎲 🎲 🎲" } ]
          _ <- env.delay $ Milliseconds 15_000.0
          env.telegram.updateKeyboard chat.id id []
        Left _ -> pure unit
    Nothing -> pure unit

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

makeUrl token tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> token <> "&tag=" <> tag
