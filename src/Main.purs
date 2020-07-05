module Main where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array.NonEmpty (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

foreign import data Bot :: Type
foreign import sendVideo :: Bot -> String -> String -> Effect Void
foreign import sendMessage :: Bot -> String -> String -> Effect Void
foreign import startBotRepl :: ({ bot :: Bot, chat :: String, text :: String } -> Effect Unit) -> Effect Unit
foreign import getApiKey :: Effect String

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

getImageUrlFromResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # map (\x -> x.data.image_mp4_url)

handleImageMsg msg = launchAff_ $ do
  apiKey <- liftEffect getApiKey
  result <- AX.request $ AX.defaultRequest { url = "https://api.giphy.com/v1/gifs/random?api_key=" <> apiKey <> "&tag=cat", 
                                             method = Left GET, 
                                             responseFormat = ResponseFormat.json }
  case getImageUrlFromResponse result of
    Right video -> liftEffect $ sendVideo msg.bot msg.chat video
    Left error -> liftEffect $ sendMessage msg.bot msg.chat ("Bot error (" <> error <> ")")

handleMsg msg =
  case match (unsafeRegex "/[^@]+" noFlags) msg.text >>= head of
    Just "/cat" -> handleImageMsg msg
    _ -> pure unit

main :: Effect Unit
main = launchAff_ $ do
  _ <- liftEffect $ startBotRepl (\msg -> handleMsg msg)
  log $ "Bot started..."
