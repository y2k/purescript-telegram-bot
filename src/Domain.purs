module Domain (update, Cmd(..)) where

import Prelude

import Affjax as AX
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toMaybe)
import Data.Time.Duration (Milliseconds(..))
import Common

data Cmd =
    SendVideo { chat :: Int, msgId :: (Maybe Int), url :: String, caption :: (Maybe String), keyboard :: (Array { text :: String, callback_data :: String }), f :: (Int -> Array Cmd) }
  | SendMessage { chatId :: Int, text :: String }
  | DownloadJson { url :: String, f :: (Either AX.Error { body :: Json } -> Array Cmd) }
  | DeleteMessage { chatId :: Int, messageId :: Int }
  | EditMessageButtons { chatId :: Int, messageId :: Int, keyboard :: Array { text :: String, callback_data :: String } }
  | EditVideo { chatId :: Int, messageId :: Int, url :: String, keyboard :: Array { text :: String, callback_data :: String } }
  | Delay { duration :: Milliseconds, commands :: (Array Cmd) }

update env msg =
  let mkUrl tag = makeUrl env.apiKey tag in
  case toMaybe msg.message of
    Just message -> handleButtonUpdate env msg message
    Nothing ->
      case toMaybe msg.chat of
        Just chat ->
          case toMaybe msg.new_chat_member of
            Just x -> 
              case toMaybe x.username of
                Just name -> handleSignupUpdate env chat name msg
                Nothing -> []
            Nothing -> handleCommandUpdate env msg chat
        Nothing -> []

handleCommandUpdate env msg chat =
  case tryExtractCommand msg of
    Just "/start" -> [ SendMessage { chatId: chat.id, text: "/cat - 😸\n/dog - 🐶" } ]
    Just "/cat" -> uploadGifToChat env "cat" chat msg.from
    Just "/dog" -> uploadGifToChat env "puppy" chat msg.from
    Just "/test_login" ->
      let msgId = toMaybe msg.message_id # maybe 0 identity in 
      [ DownloadJson { url: (makeUrl env.apiKey "cat"), f: (onImageJsonLoadedForNewUser 5 chat.id "username" msgId) } ]
    _ -> []

handleButtonUpdate env updateMessage message =
  case (unpackData <$> toMaybe updateMessage.data) of
    Just [ "reroll", _, tag, strTime ] ->
      [ DownloadJson { url: (makeUrl env.apiKey tag), f: (onVideoLoadedForEdit env tag message) } ]
    _ -> []

onVideoLoadedForEdit env tag message response =
  case getImageUrlFromResponse response of
    Right url ->
      let rerollButton = { text: "🎲 🎲 🎲", callback_data: packData "reroll" message.from tag env.now } in
      [ EditVideo { chatId: message.chat.id, messageId: message.message_id, url: url, keyboard: [ rerollButton ] } ]
    Left error -> [ SendMessage { chatId: message.chat.id, text: error } ]

handleSignupUpdate { apiKey } chat name msg =
  let msgId = toMaybe msg.message_id # maybe 0 identity in
  [ DownloadJson { url: (makeUrl apiKey "cat"), f: (onImageJsonLoadedForNewUser 30 chat.id name msgId) } ]

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

getImageUrlFromResponse response =
  response
    # lmap AX.printError
    >>= (\ { body } -> parseImageJson body)
    # map (\x -> x.data.image_mp4_url)

deleteMessageAfterTimeout chatId timeout msgId =
  [ Delay { duration: (millisecondsFromSeconds timeout), commands: [ DeleteMessage { chatId: chatId, messageId: msgId } ] } ]

onImageJsonLoadedForNewUser timeout chatId username msgId response =
  case getImageUrlFromResponse response of
    Right url -> 
      [ SendVideo 
          { chat: chatId 
          , msgId: (Just msgId) 
          , url: url 
          , caption: (Just $ "@" <> username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show timeout) <> " секунд 😸")
          , keyboard: []
          , f: (deleteMessageAfterTimeout chatId timeout) } ]
    Left error -> []

uploadGifToChat env tag chat from =
  [ DownloadJson { url: (makeUrl env.apiKey tag), f: (onVideoLoaded env tag chat from) } ]

onVideoLoaded env tag chat from response =
  let deleteKeyBoardByTimeout messageId =
        [ Delay 
          { duration: (Milliseconds 15_000.0)
          , commands : [ EditMessageButtons { chatId: chat.id, messageId: messageId, keyboard: [] } ] } ] 
  in
  case getImageUrlFromResponse response of
    Right url ->
      let rerollButton = { text: "🎲 🎲 🎲", callback_data: packData "reroll" from tag env.now } in
      [ SendVideo { chat: chat.id, msgId: Nothing, url: url, caption: Nothing, keyboard: [ rerollButton ], f: deleteKeyBoardByTimeout } ]
    Left error -> [ SendMessage { chatId: chat.id, text: error } ]

makeUrl apiKey tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> apiKey <> "&tag=" <> tag
