module Domain (update, Cmd(..)) where

import Prelude

import Affjax as AX
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Array (concat)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (toNumber)
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

update env@{ apiKey } msg =
  let mkUrl tag = makeUrl apiKey tag in
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
  case (unpackButtonData <$> toMaybe msg.data) of
    Just [ "2", _, tag, count, strTime ] ->
      case deserializeDateTime strTime of
        Just msgTime | timeInRange env.now msgTime (Milliseconds 5_000.0) ->
          concat [
            [ DeleteMessage message.chat.id message.message_id ]
            , uploadGifToChat env tag ((toIntOrZero count) - 1) message.chat message.from ]
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

onImageJsonLoaded chat response =
  case getImageUrlFromResponse response of
    Right url -> [ SendVideo chat Nothing url Nothing [] (\_ -> []) ]
    Left error -> [ SendMessage chat error ]

deleteMessageAfterTimeout chatId timeout msgId =
  [ Delay (Milliseconds (1_000.0 * (toNumber timeout))) [ DeleteMessage chatId msgId ] ]

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
          let button = { text: "🎲 🎲 🎲 ", callback_data: "2|" <> (show from.id) <> "|" <> tag <> "|" <> (show count) <> "|" <> (serializeDateTime env.now) } in
          [ SendVideo chat.id Nothing url Nothing [ button ] (\_ -> []) ]
    Left error -> [ SendMessage chat.id error ]

makeUrl apiKey tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> apiKey <> "&tag=" <> tag
