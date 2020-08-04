module Domain2 (update) where

import Prelude

import Common (packData', tryExtractCommand, unpackData')
import Data.Argonaut (Json, decodeJson)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, notNull, null)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)

update :: _ -> _ -> Aff Unit
update env msg =
  case tryExtractCommand msg of
    Just "/cat" -> sendVideo env msg "cat"
    Just "/dog" -> sendVideo env msg "puppy"
    Nothing -> update2 env msg
    _ -> pure unit

update2 :: _ -> _ -> Aff Unit
update2 env msg =
  case toMaybe msg.message of
    Just message -> 
      case toMaybe msg.data of
        Just data' -> do
          case unpackData' data' of
            [ "reroll", tag ] -> do
              let url = makeUrl env.token tag
              json <- env.downloadJson url
              case parseImageJson json of
                Right info ->
                  env.telegram.editVideo 
                    { chat: message.chat.id 
                    , messageId: message.message_id 
                    , url: info.data.image_mp4_url 
                    , keyboard: [ { callback_data: (packData' "reroll" tag), text: "🎲 🎲 🎲" } ] }
                Left _ -> pure unit
            _ -> pure unit
        Nothing -> pure unit
    Nothing -> 
      case toMaybe msg.new_chat_member of
        Just newChatMember ->
          case toMaybe msg.chat of
            Just chat ->
              case toMaybe msg.message_id of
                Just message_id -> do
                  let username = 
                        case toMaybe newChatMember.username of
                          Just username -> "@" <> username
                          Nothing -> newChatMember.first_name
                  let tag = "cat"
                  let url = makeUrl env.token tag
                  json <- env.downloadJson url
                  case parseImageJson json of
                    Right info -> do
                      let timeout = 30
                      let caption = username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show timeout) <> " секунд 😸"
                      videoMsgId <-
                        env.telegram.sendVideo
                          { chat: chat.id
                          , reply_message_id: notNull message_id
                          , url: info.data.image_mp4_url 
                          , caption: notNull caption
                          , keyboard: [] }
                      _ <- env.delay $ Milliseconds $ toNumber (timeout * 1000)
                      _ <- env.telegram.deleteMessage { chat: chat.id, message_id: videoMsgId }
                      pure unit
                    Left _ -> pure unit
                Nothing -> pure unit
            Nothing -> pure unit
        Nothing -> pure unit

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
              , keyboard: [ { callback_data: (packData' "reroll" tag), text: "🎲 🎲 🎲" } ] }
          _ <- env.delay $ Milliseconds 15_000.0
          env.telegram.updateKeyboard { chat: chat.id, messageId: id, keyboard: [] }
        Left _ -> pure unit
    Nothing -> pure unit

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

makeUrl token tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> token <> "&tag=" <> tag
