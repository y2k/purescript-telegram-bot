module Test.Domain (main) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Main (emptyDecorator, handleUpdate)
import Test.Assert (assertEqual)
import TestUtils (runTest, unsafeParseJson, unsafeToJson)
import TestUtils as T

main :: Effect Unit
main = do
  runTest "Domain.testReroll" do
    assertTelegram
      """{ "id": "1038758113615406861", "from": { "id": 241854720, "is_bot": false, "first_name": "Igor", "username": "angmarr", "language_code": "en" }, "message": { "message_id": 2883, "from": { "id": 300777612, "is_bot": true, "first_name": "DEBUG", "username": "debug3bot" }, "chat": { "id": 241854720, "first_name": "Igor", "username": "angmarr", "type": "private" }, "date": 1595600306, "animation": { "file_name": "giphy.mp4", "mime_type": "video/mp4", "duration": 4, "width": 480, "height": 480, "file_id": "CgACAgQAAxkDAAILQ18a7bJ1qnu7zTnXDhGs0XZTqvi3AAJWAgACflDdUKs1tOPjqZ07GgQ", "file_unique_id": "AgADVgIAAn5Q3VA", "file_size": 2363111 }, "document": { "file_name": "giphy.mp4", "mime_type": "video/mp4", "file_id": "CgACAgQAAxkDAAILQ18a7bJ1qnu7zTnXDhGs0XZTqvi3AAJWAgACflDdUKs1tOPjqZ07GgQ", "file_unique_id": "AgADVgIAAn5Q3VA", "file_size": 2363111 }, "reply_markup": { "inline_keyboard": [] } }, "chat_instance": "7027187588705046566", "data": "6|reroll|puppy" }"""
      [ "ev:{\"chat_id\":241854720,\"message_id\":2883,\"url\":\"_VIDEO_URL_\",\"keyboard\":[{\"callback_data\":\"6|reroll|puppy\",\"text\":\"🎲 🎲 🎲\"}]}" ]

  runTest "Domain.testDogCommand" do
    assertTelegram
      """{ "message_id": 2882, "from": { "id": 241854720, "is_bot": false, "first_name": "Igor", "username": "angmarr", "language_code": "en" }, "chat": { "id": 241854720, "first_name": "Igor", "username": "angmarr", "type": "private" }, "date": 1595600304, "text": "/dog", "entities": [ { "offset": 0, "length": 4, "type": "bot_command" } ] }"""
      [ "sv:{\"chat_id\":241854720,\"reply_to_message_id\":null,\"url\":\"_VIDEO_URL_\",\"caption\":null,\"keyboard\":[{\"callback_data\":\"6|reroll|puppy\",\"text\":\"🎲 🎲 🎲\"}]}"
      , "d:(Milliseconds 15000.0)"
      , "uk:{\"chat_id\":241854720,\"message_id\":42,\"keyboard\":[]}" ]

  runTest "Domain.testLongDogCommand" do
    assertTelegram
      """{ "message_id": 2882, "from": { "id": 241854720, "is_bot": false, "first_name": "Igor", "username": "angmarr", "language_code": "en" }, "chat": { "id": 241854720, "first_name": "Igor", "username": "angmarr", "type": "private" }, "date": 1595600304, "text": "/dog@relaxcats_bot", "entities": [ { "offset": 0, "length": 4, "type": "bot_command" } ] }"""
      [ "sv:{\"chat_id\":241854720,\"reply_to_message_id\":null,\"url\":\"_VIDEO_URL_\",\"caption\":null,\"keyboard\":[{\"callback_data\":\"6|reroll|puppy\",\"text\":\"🎲 🎲 🎲\"}]}"
      , "d:(Milliseconds 15000.0)"
      , "uk:{\"chat_id\":241854720,\"message_id\":42,\"keyboard\":[]}" ]

  runTest "Domain.testUserLogin" do
    assertTelegram
      """{ "message_id": 209149, "from": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "chat": { "id": -1001130908027, "title": "Programming Offtop", "username": "pofftop", "type": "supergroup" }, "date": 1595360387, "new_chat_participant": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_member": { "id": 714583317, "is_bot": false, "username": "no_name", "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_members": [ { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" } ] }"""
      [ "sv:{\"chat_id\":-1001130908027,\"reply_to_message_id\":209149,\"url\":\"_VIDEO_URL_\",\"caption\":\"@no_name, докажите что вы человек.\\nНапишите что происходит на картинке. У вас 30 секунд 😸\",\"keyboard\":[]}"
      , "d:(Milliseconds 30000.0)"
      , "dm:{\"chat_id\":-1001130908027,\"message_id\":42}" ]

  runTest "Domain.testUserLoginWithoutName" do
    assertTelegram
      """{ "message_id": 209149, "from": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "chat": { "id": -1001130908027, "title": "Programming Offtop", "username": "pofftop", "type": "supergroup" }, "date": 1595360387, "new_chat_participant": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_member": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_members": [ { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" } ] }"""
      [ "sv:{\"chat_id\":-1001130908027,\"reply_to_message_id\":209149,\"url\":\"_VIDEO_URL_\",\"caption\":\"Anatoliy, докажите что вы человек.\\nНапишите что происходит на картинке. У вас 30 секунд 😸\",\"keyboard\":[]}"
      , "d:(Milliseconds 30000.0)"
      , "dm:{\"chat_id\":-1001130908027,\"message_id\":42}" ]

assertTelegram :: String -> Array String -> Effect Unit
assertTelegram msgJson expectedResponses = do
  log <- T.newQueue
  msg <- unsafeParseJson msgJson
  let env =
        { token: ""
        , downloadJson: \_ -> T.pureA $ T.unsafeParse """{ "data": { "images": { "original": { "mp4": "_VIDEO_URL_" } } } }"""
        , delay: \x -> T.push ("d:" <> (show x)) log *> pure unit # liftEffect
        , telegram:
            { sendVideo: \x -> (unsafeToJson x >>= (\x -> T.push ("sv:" <> x) log)) *> pure { message_id : 42 } # liftEffect
            , editMessageMedia: \x -> (unsafeToJson x >>= (\x -> T.push ("ev:" <> x) log)) *> pure unit # liftEffect
            , editMessageReplyMarkup: \x -> (unsafeToJson x >>= (\x -> T.push ("uk:" <> x) log)) *> pure unit # liftEffect
            , deleteMessage: \x -> (unsafeToJson x >>= (\x -> T.push ("dm:" <> x) log)) *> pure unit # liftEffect
            , sendMessage: \x -> (unsafeToJson x >>= (\x -> T.push ("sm:" <> x) log)) *> pure { message_id : 42 } } }

  launchAff_ (handleUpdate emptyDecorator emptyDecorator env msg)

  logA <- T.toArray log
  assertEqual
    { expected: expectedResponses
    , actual: logA }
