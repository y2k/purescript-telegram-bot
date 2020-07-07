module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (concat)
import Data.Array.NonEmpty (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

data Cmd =
    SendVideo String (Maybe Int) String (Maybe String)
  | SendMessage String String
  | DownloadJson String (Either AX.Error { body :: Json } -> Array Cmd)

getImageUrlFromResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # map (\x -> x.data.image_mp4_url)

onImageJsonLoaded chat response =
  case getImageUrlFromResponse response of
    Right url -> [ SendVideo chat Nothing url Nothing ]
    Left error -> [ SendMessage chat error ]

onImageJsonLoadedForNewUser chat username msgId response =
  case getImageUrlFromResponse response of
    Right url -> [ SendVideo chat (Just msgId) url (Just $ username <> ", докажите что вы человек. Опишите что изображено на картинке. У вас есть 30 секунд 😸") ]
    Left error -> []

update { apiKey } msg =
  let url = "https://api.giphy.com/v1/gifs/random?api_key=" <> apiKey <> "&tag=cat" in
  case toMaybe msg.regUserName of
    Just name -> [ DownloadJson url (onImageJsonLoadedForNewUser msg.chat name msg.id) ]
    Nothing ->
      let telegramCmd = match (unsafeRegex "/[^@]+" noFlags) msg.text >>= head in
      case telegramCmd of
        Just "/cat" -> [ DownloadJson url (onImageJsonLoaded msg.chat) ]
        _ -> []

foreign import data Bot :: Type
foreign import sendVideo :: Bot -> String -> Nullable Int -> String-> Nullable String -> Effect Void
foreign import sendMessage :: Bot -> String -> String -> Effect Void
foreign import startBotRepl :: ({ bot :: Bot, chat :: String, text :: String, id :: Int, regUserName :: Nullable String } -> Effect Unit) -> Effect Unit
foreign import getApiKey :: Effect String

executeCmd :: Bot -> Cmd -> Aff (Array Cmd)
executeCmd bot cmd =
  case cmd of
    SendVideo chat msgId url caption -> sendVideo bot chat (toNullable msgId) url (toNullable caption) *> pure [] # liftEffect
    SendMessage chat text -> sendMessage bot chat text *> pure [] # liftEffect
    DownloadJson url f ->
      do
        result <-
          AX.defaultRequest { url = url, method = Left GET, responseFormat = ResponseFormat.json }
          # AX.request
        pure $ f (map (\r -> { body : r.body }) result)

executeCmds :: Bot -> Array Cmd -> Aff Unit
executeCmds bot cmds =
  case cmds of
    [] -> pure unit
    _ ->
      do
        let effects = cmds # map (executeCmd bot)
        newCmds <- sequence effects
        executeCmds bot (concat newCmds)

main :: Effect Unit
main = do
  startBotRepl (\msg -> launchAff_ $ do
    apiKey <- liftEffect getApiKey
    let cmds = update { apiKey : apiKey } msg
    executeCmds msg.bot cmds)
  log $ "Bot started..."
