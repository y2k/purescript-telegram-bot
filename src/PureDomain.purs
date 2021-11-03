module PureDomain where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Either (Either)
import Data.Time.Duration (Seconds(..))

captchaTimeout = 30
limitCount = 2
limitPerSedonds = Seconds 15.0

parseImageJson :: Json -> Either JsonDecodeError { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

makeUrl token tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> token <> "&tag=" <> tag
