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

userFromJson :: Json -> Either String { data :: { image_url :: String } }
userFromJson = decodeJson

main :: Effect Unit
main = void $ launchAff $ do
  result <- AX.request (AX.defaultRequest { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat", 
                                            method = Left GET, 
                                            responseFormat = ResponseFormat.json })
  case result of
    Left e -> log $ "GET /api response failed to decode: " <> AX.printError e
    Right { body } ->
      case userFromJson body of
        Right x -> log $ "Image URL: " <> x.data.image_url
        Left e -> log  $ "Error: " <> e
