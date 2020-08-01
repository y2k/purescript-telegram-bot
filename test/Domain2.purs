module Test.Domain2 (main) where

import Prelude

import Data.Argonaut (jsonParser)
import Data.Either (fromRight)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Main (unsafeParseJson, unsafeToJson)
import Partial.Unsafe (unsafePartial)
import Domain2 as D
import Queue as Q
import Test.Assert (assertEqual)

testReroll =
  assertTelegram
    """{ "id": "1038758113615406861", "from": { "id": 241854720, "is_bot": false, "first_name": "Igor", "username": "angmarr", "language_code": "en" }, "message": { "message_id": 2883, "from": { "id": 300777612, "is_bot": true, "first_name": "DEBUG", "username": "debug3bot" }, "chat": { "id": 241854720, "first_name": "Igor", "username": "angmarr", "type": "private" }, "date": 1595600306, "animation": { "file_name": "giphy.mp4", "mime_type": "video/mp4", "duration": 4, "width": 480, "height": 480, "file_id": "CgACAgQAAxkDAAILQ18a7bJ1qnu7zTnXDhGs0XZTqvi3AAJWAgACflDdUKs1tOPjqZ07GgQ", "file_unique_id": "AgADVgIAAn5Q3VA", "file_size": 2363111 }, "document": { "file_name": "giphy.mp4", "mime_type": "video/mp4", "file_id": "CgACAgQAAxkDAAILQ18a7bJ1qnu7zTnXDhGs0XZTqvi3AAJWAgACflDdUKs1tOPjqZ07GgQ", "file_unique_id": "AgADVgIAAn5Q3VA", "file_size": 2363111 }, "reply_markup": { "inline_keyboard": [] } }, "chat_instance": "7027187588705046566", "data": "5|reroll|puppy" }"""
    [ "ev:{\"chat\":241854720,\"messageId\":2883,\"url\":\"_VIDEO_URL_\",\"keyboard\":[{\"callback_data\":\"5|reroll|puppy\",\"text\":\"🎲 🎲 🎲\"}]}" ]

testDogCommand =
  assertTelegram
    """{ "message_id": 2882, "from": { "id": 241854720, "is_bot": false, "first_name": "Igor", "username": "angmarr", "language_code": "en" }, "chat": { "id": 241854720, "first_name": "Igor", "username": "angmarr", "type": "private" }, "date": 1595600304, "text": "/dog", "entities": [ { "offset": 0, "length": 4, "type": "bot_command" } ] }"""
    [ "sv:{\"chat\":241854720,\"reply_message_id\":null,\"url\":\"_VIDEO_URL_\",\"caption\":null,\"keyboard\":[{\"callback_data\":\"5|reroll|puppy\",\"text\":\"🎲 🎲 🎲\"}]}"
    , "d:(Milliseconds 15000.0)"
    , "uk:{\"chat\":241854720,\"messageId\":0,\"keyboard\":[]}" ]

testUserLogin = do
  assertTelegram
    """{ "message_id": 209149, "from": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "chat": { "id": -1001130908027, "title": "Programming Offtop", "username": "pofftop", "type": "supergroup" }, "date": 1595360387, "new_chat_participant": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_member": { "id": 714583317, "is_bot": false, "username": "no_name", "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_members": [ { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" } ] }"""
    [ "sv:{\"chat\":-1001130908027,\"reply_message_id\":209149,\"url\":\"_VIDEO_URL_\",\"caption\":\"@no_name, докажите что вы человек.\\nНапишите что происходит на картинке. У вас 30 секунд 😸\",\"keyboard\":[]}" ]

testUserLoginWithoutName = do
  assertTelegram
    """{ "message_id": 209149, "from": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "chat": { "id": -1001130908027, "title": "Programming Offtop", "username": "pofftop", "type": "supergroup" }, "date": 1595360387, "new_chat_participant": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_member": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_members": [ { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" } ] }"""
    [ "sv:{\"chat\":-1001130908027,\"reply_message_id\":209149,\"url\":\"_VIDEO_URL_\",\"caption\":\"Anatoliy, докажите что вы человек.\\nНапишите что происходит на картинке. У вас 30 секунд 😸\",\"keyboard\":[]}" ]

main :: Effect Unit
main = do
  _ <- testReroll
  _ <- testDogCommand
  _ <- testUserLogin
  _ <- testUserLoginWithoutName
  pure unit

assertTelegram msgJson expectedResponses = do
  log <- Q.newQueue
  msg <- unsafeParseJson msgJson
  let env = 
        { token: ""
        , downloadJson: \_ -> pureA $ unsafeParse """{ "data": { "image_mp4_url": "_VIDEO_URL_" } }"""
        , delay: \x -> Q.push ("d:" <> (show x)) log *> pure unit # liftEffect
        , telegram:
            { sendVideo: \x -> (unsafeToJson x >>= (\x -> Q.push ("sv:" <> x) log)) *> pure 0 # liftEffect
            , editVideo: \x -> (unsafeToJson x >>= (\x -> Q.push ("ev:" <> x) log)) *> pure unit # liftEffect
            , updateKeyboard: \x -> (unsafeToJson x >>= (\x -> Q.push ("uk:" <> x) log)) *> pure unit # liftEffect } }

  launchAff_ (D.update env msg)

  logA <- Q.toArray log
  assertEqual 
    { expected: expectedResponses
    , actual: logA }

pureA :: ∀ a. a -> Aff a
pureA x = pure x
unsafeParse x = jsonParser x # unsafePartial fromRight
notImpl = throw "not implemented" # liftEffect
