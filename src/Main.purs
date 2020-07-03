module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise as P
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

-- https://www.npmjs.com/package/node-telegram-bot-api

foreign import startRepl :: ({ text :: String } -> Effect (P.Promise String)) -> Effect Unit
foreign import getApkKey :: Effect String

parseImageJson :: Json -> Either String { data :: { image_url :: String } }
parseImageJson = decodeJson

getImageUrlJsonResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # either identity (\x -> x.data.image_url)

handleMsg :: { text :: String } -> Aff String
handleMsg msg = do
  apiKey <- liftEffect getApkKey
  result <- AX.request $ AX.defaultRequest { url = "https://api.giphy.com/v1/gifs/random?api_key=" <> apiKey <> "&tag=cat", 
                                             method = Left GET, 
                                             responseFormat = ResponseFormat.json }
  pure $ getImageUrlJsonResponse result

main :: Effect Unit
main = void $ launchAff $ do
  _ <- liftEffect $ startRepl (\msg -> P.fromAff $ handleMsg msg)
  log $ "Bot started..."
