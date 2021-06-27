module PureDomain where

import Prelude

import Data.Argonaut (Json, decodeJson)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Map (Map)
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
