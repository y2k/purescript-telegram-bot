module Domain (update, Cmd(..)) where

import Prelude

import Affjax as AX
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (concat)
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
    Just "/cat" -> uploadGifToChat env "cat" 2 chat msg.from
    Just "/dog" -> uploadGifToChat env "puppy" 2 chat msg.from
    Just "/test_login" -> [ DownloadJson (makeUrl env.apiKey "cat") (onImageJsonLoadedForNewUser 5 chat.id "username" msg.id) ]
    _ -> []

handleButtonUpdate env msg message =
  let sendNext tag count = uploadGifToChat env tag ((toIntOrZero count) - 1) message.chat message.from in
  let isActual msgTime = timeInRange env.now msgTime (Milliseconds 10_000.0) in
  case (unpackData <$> toMaybe msg.data) of
    Just [ "reroll", _, tag, count, strTime ] ->
      case deserializeDateTime strTime of
        Just msgTime | isActual msgTime ->
          concat [
            [ DeleteMessage message.chat.id message.message_id ]
            , sendNext tag count ]
        _ -> []
    Just [ "more", _, tag, count, strTime ] -> 
      case deserializeDateTime strTime of
        Just msgTime | isActual msgTime -> sendNext tag count
        _ -> []
    _ -> []

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

uploadGifToChat env tag count chat from =
  [ DownloadJson (makeUrl env.apiKey tag) (onVideoLoaded env tag count chat from) ]

onVideoLoaded env tag count chat from response =
  case getImageUrlFromResponse response of
    Right url ->
      case count of
        0 -> [ SendVideo chat.id Nothing url Nothing [] (\_ -> []) ]
        _ ->
          let rerollButton = { text: "🎲 🎲 🎲", callback_data: packData "reroll" from tag count env.now } in
          let moreButton = { text: "MORE", callback_data: packData "more" from tag count env.now } in
          [ SendVideo chat.id Nothing url Nothing [ rerollButton, moreButton ] (\_ -> []) ]
    Left error -> [ SendMessage chat.id error ]

makeUrl apiKey tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> apiKey <> "&tag=" <> tag
