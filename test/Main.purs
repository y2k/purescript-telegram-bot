module Test.Main (main) where

import Domain
import Prelude
import Test.Assert

import Data.Either (Either(..), fromRight)
import Data.Nullable (notNull, null)
import Effect (Effect)
import Effect.Now (nowDateTime)
import Data.Argonaut.Parser (jsonParser)
import Partial.Unsafe (unsafePartial)

test_cat env = do
  let msg = { from: { id: 0 }, chat: notNull { id: 0 }, text: "/cat", id: 0, regUserName: null, data: null, message: null }
  case update env msg of
    [ DownloadJson url f ] -> do
      assertEqual 
        { expected: "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=_KEY_&tag=cat"
        , actual: url }
      let json = jsonParser "{ \"data\": { \"image_mp4_url\": \"_VIDEO_URL_\" } }" # unsafePartial fromRight
      case f $ Right { body: json } of
        [ SendVideo _ _ url _ _ _ ] -> assertEqual { expected: "_VIDEO_URL_", actual: url }
        _ -> assertTrue false
    _ -> assertTrue false

main :: Effect Unit
main = do
  now <- nowDateTime
  test_cat { apiKey: "_KEY_", now: now }
