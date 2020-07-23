module Domain2 (update) where

import Prelude

import Common (packData', unpackData')
import Data.Argonaut (Json, decodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, notNull, null)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff)

update :: _ -> _ -> Aff Unit
update env msg =
  case toMaybe msg.text of
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
                    message.chat.id 
                    message.message_id 
                    info.data.image_mp4_url 
                    [ { callback_data: (packData' "reroll" tag), text: "🎲 🎲 🎲" } ]
                Left _ -> pure unit
            _ -> pure unit
        Nothing -> pure unit
    Nothing -> 
      case toMaybe msg.new_chat_member of
        Just newChatMember ->
          case toMaybe newChatMember.username of
            Just username ->
              case toMaybe msg.chat of
                Just chat -> do
                  let tag = "cat"
                  let url = makeUrl env.token tag
                  json <- env.downloadJson url
                  case parseImageJson json of
                    Right info -> do
                      let timeout = 30
                      let caption = "@" <> username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show timeout) <> " секунд 😸"
                      _ <-
                        env.telegram.sendVideo
                          { chat: chat.id
                          , url: info.data.image_mp4_url 
                          , caption: notNull caption
                          , keyboard: [] }
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
              , url: info.data.image_mp4_url
              , caption: null
              , keyboard: [ { callback_data: (packData' "reroll" tag), text: "🎲 🎲 🎲" } ] }
          _ <- env.delay $ Milliseconds 15_000.0
          env.telegram.updateKeyboard chat.id id []
        Left _ -> pure unit
    Nothing -> pure unit

parseImageJson :: Json -> Either String { data :: { image_mp4_url :: String } }
parseImageJson = decodeJson

makeUrl token tag = "https://api.giphy.com/v1/gifs/random?rating=pg&api_key=" <> token <> "&tag=" <> tag
