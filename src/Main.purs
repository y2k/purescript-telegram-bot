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
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Control.Promise (Promise, toAffE)

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

data Cmd =
    SendVideo String (Maybe Int) String (Maybe String) (Int -> Array Cmd)
  | SendMessage String String
  | DownloadJson String (Either AX.Error { body :: Json } -> Array Cmd)
  | DeleteMessage String Int
  | Delay Milliseconds (Array Cmd)

getImageUrlFromResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # map (\x -> x.data.image_mp4_url)

onImageJsonLoaded chat response =
  case getImageUrlFromResponse response of
    Right url -> [ SendVideo chat Nothing url Nothing (\_ -> []) ]
    Left error -> [ SendMessage chat error ]

onDelayEnded chatId msgId =
  [ Delay (Milliseconds 30_000.0) [ DeleteMessage chatId msgId ] ]

onImageJsonLoadedForNewUser chatId username msgId response =
  case getImageUrlFromResponse response of
    Right url -> 
      [ SendVideo 
          chatId 
          (Just msgId) 
          url 
          (Just $ username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас 30 секунд 😸")
          (onDelayEnded chatId) ]
    Left error -> []

update { apiKey } msg =
  let mkUrl tag = makeUrl apiKey tag in
  let defUrl = mkUrl "cat" in
  case toMaybe msg.regUserName of
    Just name -> [ DownloadJson defUrl (onImageJsonLoadedForNewUser msg.chat name msg.id) ]
    Nothing ->
      let telegramCmd = match (unsafeRegex "/[^@]+" noFlags) msg.text >>= head in
      case telegramCmd of
        Just "/cat" -> [ DownloadJson defUrl (onImageJsonLoaded msg.chat) ]
        Just "/dog" -> [ DownloadJson (mkUrl "puppy") (onImageJsonLoaded msg.chat) ]
        Just "/parrot" -> [ DownloadJson (mkUrl "parrot") (onImageJsonLoaded msg.chat) ]
        Just "/animals" -> [ DownloadJson (mkUrl "cute-animals") (onImageJsonLoaded msg.chat) ]
        Just "/test_login" -> [ DownloadJson defUrl (onImageJsonLoadedForNewUser msg.chat "<user>" msg.id) ]
        _ -> []

makeUrl apiKey tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> apiKey <> "&tag=" <> tag

foreign import data Bot :: Type
foreign import sendVideo :: Bot -> String -> Nullable Int -> String-> Nullable String -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: Bot -> String -> String -> Effect Void
foreign import startBotRepl :: ({ bot :: Bot, chat :: String, text :: String, id :: Int, regUserName :: Nullable String } -> Effect Unit) -> Effect Unit
foreign import getApiKey :: Effect String
foreign import deleteMessage :: Bot -> String -> Int -> Effect Void

executeCmd :: Bot -> Cmd -> Aff (Array Cmd)
executeCmd bot cmd =
  case cmd of
    SendVideo chat msgId url caption f -> do
      msg <- toAffE $ sendVideo bot chat (toNullable msgId) url (toNullable caption)
      pure $ f msg.message_id
    SendMessage chat text -> sendMessage bot chat text *> pure [] # liftEffect
    Delay duration nextCmds -> do
      delay duration
      pure nextCmds
    DeleteMessage chat msgId -> deleteMessage bot chat msgId *> pure [] # liftEffect
    DownloadJson url f -> do
      result <-
        AX.defaultRequest { url = url, method = Left GET, responseFormat = ResponseFormat.json }
        # AX.request
      pure $ f (map (\r -> { body : r.body }) result)

executeCmds :: Bot -> Array Cmd -> Aff Unit
executeCmds bot cmds =
  case cmds of
    [] -> pure unit
    _ -> do
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
