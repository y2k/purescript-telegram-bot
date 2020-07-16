module Domain (update, Cmd(..)) where

import Prelude

import Affjax as AX
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Common

data Cmd =
    SendVideo Int (Maybe Int) String (Maybe String) (Array { text :: String, callback_data :: String }) (Int -> Array Cmd)
  | SendMessage Int String
  | DownloadJson String (Either AX.Error { body :: Json } -> Array Cmd)
  | DeleteMessage Int Int
  | EditVideo { chatId :: Int, messageId :: Int, url :: String, keyboard :: Array { text :: String, callback_data :: String } }
  | Delay Milliseconds (Array Cmd)

update env msg =
  let mkUrl tag = makeUrl env.apiKey tag in
  case toMaybe msg.message of
    Just message -> handleButtonUpdate env msg message
    Nothing ->
      case toMaybe msg.chat of
        Just chat ->
          case toMaybe msg.regUserName of
            Just name -> handleSignupUpdate env chat name msg
            Nothing -> handleCommandUpdate env msg chat
        Nothing -> []

handleCommandUpdate env msg chat =
  case tryExtractCommand msg of
    Just "/start" -> [ SendMessage chat.id "/cat - 😸\n/dog - 🐶" ]
    Just "/cat" -> uploadGifToChat env "cat" chat msg.from
    Just "/dog" -> uploadGifToChat env "puppy" chat msg.from
    Just "/test_login" -> [ DownloadJson (makeUrl env.apiKey "cat") (onImageJsonLoadedForNewUser 5 chat.id "username" msg.id) ]
    _ -> []

handleButtonUpdate env updateMessage message =
  let isActual msgTime = timeInRange env.now msgTime (Milliseconds 10_000.0) in
  case (unpackData <$> toMaybe updateMessage.data) of
    Just [ "reroll", _, tag, strTime ] ->
      case deserializeDateTime strTime of
        Just msgTime | isActual msgTime ->
            [ DownloadJson (makeUrl env.apiKey tag) (onVideoLoadedForEdit env tag message) ]
        _ -> []
    _ -> []

onVideoLoadedForEdit env tag message response =
  case getImageUrlFromResponse response of
    Right url ->
      let rerollButton = { text: "🎲 🎲 🎲", callback_data: packData "reroll" message.from tag env.now } in
      [ EditVideo { chatId: message.chat.id, messageId: message.message_id, url: url, keyboard: [ rerollButton ] } ]
    Left error -> [ SendMessage message.chat.id error ]

handleSignupUpdate { apiKey } chat name msg =
  [ DownloadJson (makeUrl apiKey "cat") (onImageJsonLoadedForNewUser 30 chat.id name msg.id) ]

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

getImageUrlFromResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # map (\x -> x.data.image_mp4_url)

deleteMessageAfterTimeout chatId timeout msgId =
  [ Delay (millisecondsFromSeconds timeout) [ DeleteMessage chatId msgId ] ]

onImageJsonLoadedForNewUser timeout chatId username msgId response =
  case getImageUrlFromResponse response of
    Right url -> 
      [ SendVideo 
          chatId 
          (Just msgId) 
          url 
          (Just $ "@" <> username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show timeout) <> " секунд 😸")
          []
          (deleteMessageAfterTimeout chatId timeout) ]
    Left error -> []

uploadGifToChat env tag chat from =
  [ DownloadJson (makeUrl env.apiKey tag) (onVideoLoaded env tag chat from) ]

onVideoLoaded env tag chat from response =
  case getImageUrlFromResponse response of
    Right url ->
      let rerollButton = { text: "🎲 🎲 🎲", callback_data: packData "reroll" from tag env.now } in
      [ SendVideo chat.id Nothing url Nothing [ rerollButton ] (\_ -> []) ]
    Left error -> [ SendMessage chat.id error ]

makeUrl apiKey tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> apiKey <> "&tag=" <> tag
