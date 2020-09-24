module Domain (update) where

import Prelude

import Common as C
import Data.Argonaut (Json, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, notNull, null)
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple3)
import Effect.Aff (Aff)

update :: _ -> _ -> Aff Unit
update env msg =
  case C.tryExtractCommand msg of
    Just text -> handleCommand env msg text
    Nothing ->
      case toMaybe msg.message of
        Just message ->
          case toMaybe msg.data of
            Just data' -> do
              case C.unpackData' data' of
                [ "reroll", tag ] -> handleReroll env tag message
                _ -> pure unit
            Nothing -> pure unit
        Nothing ->
          case tuple3 (toMaybe msg.chat) (toMaybe msg.message_id) (toMaybe msg.new_chat_member) of
            Tuple (Just chat) (Tuple (Just message_id) (Tuple (Just newChatMember) unit)) ->
              handleLogin env chat message_id newChatMember
            _ -> pure unit

handleCommand env msg text =
  case text of
    "/cat" -> sendVideo env msg "cat"
    "/dog" -> sendVideo env msg "puppy"
    _ -> pure unit

handleLogin env chat message_id newChatMember = do
  let username =
        case toMaybe newChatMember.username of
          Just username -> "@" <> username
          Nothing -> newChatMember.first_name
  let tag = "cat"
  let url = makeUrl env.token tag
  json <- env.downloadJson url
  case parseImageJson json of
    Right info -> do
      let timeout = 30.0
      let caption = username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show timeout) <> " секунд 😸"
      videoMsgId <-
        env.telegram.sendVideo
          { chat: chat.id
          , reply_message_id: notNull message_id
          , url: info.data.image_mp4_url
          , caption: notNull caption
          , keyboard: [] }
      _ <- env.delay $ fromDuration $ Seconds timeout
      _ <- env.telegram.deleteMessage { chat: chat.id, message_id: videoMsgId }
      pure unit
    Left _ -> pure unit

handleReroll env tag message = do
  let url = makeUrl env.token tag
  json <- env.downloadJson url
  case parseImageJson json of
    Right info ->
      env.telegram.editVideo
        { chat: message.chat.id
        , messageId: message.message_id
        , url: info.data.image_mp4_url
        , keyboard: [ { callback_data: (C.packData' "reroll" tag), text: "🎲 🎲 🎲" } ] }
    Left _ -> pure unit

sendVideo env msg tag =
  case toMaybe msg.chat of
    Just chat -> do
      let url = makeUrl env.token tag
      json <- env.downloadJson url
      case parseImageJson json of
        Right info -> do
          id <-
            env.telegram.sendVideo
              { chat: chat.id
              , reply_message_id: null
              , url: info.data.image_mp4_url
              , caption: null
              , keyboard: [ { callback_data: (C.packData' "reroll" tag), text: "🎲 🎲 🎲" } ] }
          _ <- env.delay $ fromDuration $ Seconds 15.0
          env.telegram.updateKeyboard { chat: chat.id, messageId: id, keyboard: [] }
        Left _ -> pure unit
    Nothing -> pure unit

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

makeUrl token tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> token <> "&tag=" <> tag
