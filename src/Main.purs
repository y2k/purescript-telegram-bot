module Main where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (concat)
import Data.Array.NonEmpty (head)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String (Pattern(..), split)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

data Cmd =
    SendVideo Int (Maybe Int) String (Maybe String) (Int -> Array Cmd) (Array { text :: String, callback_data :: String })
  | SendMessage Int String
  | DownloadJson String (Either AX.Error { body :: Json } -> Array Cmd)
  | DeleteMessage Int Int
  | Delay Milliseconds (Array Cmd)

getImageUrlFromResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # map (\x -> x.data.image_mp4_url)

onImageJsonLoaded chat response =
  case getImageUrlFromResponse response of
    Right url -> [ SendVideo chat Nothing url Nothing (\_ -> []) [] ]
    Left error -> [ SendMessage chat error ]

onDelayEnded chatId timeout msgId =
  [ Delay (Milliseconds (1_000.0 * (toNumber timeout))) [ DeleteMessage chatId msgId ] ]

onImageJsonLoadedForNewUser timeout chatId username msgId response =
  case getImageUrlFromResponse response of
    Right url -> 
      [ SendVideo 
          chatId 
          (Just msgId) 
          url 
          (Just $ "@" <> username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show timeout) <> " секунд 😸")
          (onDelayEnded chatId timeout)
          [] ]
    Left error -> []

onVideoLoaded tag count chat from response =
  case getImageUrlFromResponse response of
    Right url ->
      case count of
        0 -> [ SendVideo chat.id Nothing url Nothing (\_ -> []) [] ]
        _ ->
          let button = { text: "🎲 🎲 🎲 ", callback_data: "1|" <> (show from.id) <> "|" <> tag <> "|" <> (show count) } in
          [ SendVideo chat.id Nothing url Nothing (\_ -> []) [ button ] ]
    Left error -> [ SendMessage chat.id error ]

loadImageForVideo env tag count chat from =
  [ DownloadJson (makeUrl env.apiKey tag) (onVideoLoaded tag count chat from) ]

reroll env data' msg =
  case split (Pattern "|") data' of
    [ "1", _, tag, count ] -> 
      [ DeleteMessage msg.chat.id msg.message_id ] <> (loadImageForVideo env tag ((toIntOrDef count) - 1) msg.chat msg.from)
    _ -> []

update env@{ apiKey } msg =
  let mkUrl tag = makeUrl apiKey tag in
  case toMaybe msg.message of
    Just message ->
      case toMaybe msg.data of
        Just data' -> reroll env data' message
        Nothing -> []
    Nothing ->
      case toMaybe msg.chat of
        Nothing -> []
        Just chat ->
          case toMaybe msg.regUserName of
            Just name -> [ DownloadJson (mkUrl "cat") (onImageJsonLoadedForNewUser 30 chat.id name msg.id) ]
            Nothing ->
              let telegramCmd = match (unsafeRegex "/[^@]+" noFlags) msg.text >>= head in
              case telegramCmd of
                Just "/cat" -> loadImageForVideo env "cat" 2 chat msg.from
                Just "/dog" -> loadImageForVideo env "puppy" 2 chat msg.from
                Just "/test_login" -> [ DownloadJson (mkUrl "cat") (onImageJsonLoadedForNewUser 5 chat.id "username" msg.id) ]
                _ -> []

makeUrl apiKey tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> apiKey <> "&tag=" <> tag

toIntOrDef x =
  fromString x # maybe 0 identity

foreign import data Bot :: Type
foreign import getApiKey :: Effect String
foreign import sendVideo :: Bot -> Int -> Nullable Int -> String-> Nullable String -> Array { text :: String, callback_data :: String } -> Effect (Promise { message_id :: Int })
foreign import sendMessage :: Bot -> Int -> String -> Effect Void
foreign import deleteMessage :: Bot -> Int -> Int -> Effect Void
foreign import startBotRepl :: ({ from :: { id :: Int }, bot :: Bot, chat :: Nullable { id :: Int }, text :: String, id :: Int, regUserName :: Nullable String, data :: Nullable String, message :: Nullable { message_id :: Int, chat :: { id :: Int }, from :: { id :: Int } } } -> Effect Unit) -> Effect Unit

executeCmd :: Bot -> Cmd -> Aff (Array Cmd)
executeCmd bot cmd =
  case cmd of
    SendVideo chat msgId url caption f keyboard -> do
      msg <- toAffE $ sendVideo bot chat (toNullable msgId) url (toNullable caption) keyboard
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
