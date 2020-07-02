module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class.Console (log)

-- https://www.npmjs.com/package/node-telegram-bot-api
foreign import test :: Int -> Effect Int

userFromJson :: Json -> Either String { data :: { image_url :: String } }
userFromJson = decodeJson

main :: Effect Unit
main = do
  value <- test 0
  log $ show value
-- main = void $ launchAff $ do
--   value <- T.test 0
--   log $ show value
--   result <- AX.request (AX.defaultRequest { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat", 
--                                             method = Left GET, 
--                                             responseFormat = ResponseFormat.json })
--   case result of
--     Left e -> log $ "GET /api response failed to decode: " <> AX.printError e
--     Right { body } ->
--       case userFromJson body of
--         Right x -> log $ "Image URL: " <> x.data.image_url
--         Left e -> log $ "Error: " <> e
