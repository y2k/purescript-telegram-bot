module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise as P
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

foreign import data Bot :: Type
foreign import sendVideo :: Bot -> String -> String -> Effect Void
foreign import sendMessage :: Bot -> String -> String -> Effect Void
foreign import startBotRepl :: ({ bot :: Bot, chat :: String } -> Effect _) -> Effect Unit
foreign import getApiKey :: Effect String

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

getImageUrlFromResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # map (\x -> x.data.image_mp4_url)

handleMsg msg = do
  apiKey <- liftEffect getApiKey
  result <- AX.request $ AX.defaultRequest { url = "https://api.giphy.com/v1/gifs/random?api_key=" <> apiKey <> "&tag=cat", 
                                             method = Left GET, 
                                             responseFormat = ResponseFormat.json }
  case getImageUrlFromResponse result of
    Right video -> pure $ sendVideo msg.bot msg.chat video
    Left error -> pure $ sendMessage msg.bot msg.chat error

main :: Effect Unit
main = void $ launchAff $ do
  _ <- liftEffect $ startBotRepl (\msg -> P.fromAff $ handleMsg msg)
  log $ "Bot started..."
