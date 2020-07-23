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

main :: Effect Unit
main = do
  log <- Q.newQueue
  msg <- unsafeParseJson """{ "message_id": 209149, "from": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "chat": { "id": -1001130908027, "title": "Programming Offtop", "username": "pofftop", "type": "supergroup" }, "date": 1595360387, "new_chat_participant": { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_member": { "id": 714583317, "is_bot": false, "username": "no_name", "first_name": "Anatoliy", "last_name": "Kernokus" }, "new_chat_members": [ { "id": 714583317, "is_bot": false, "first_name": "Anatoliy", "last_name": "Kernokus" } ] }"""
  let env = 
        { token: ""
        , delay: \_ -> notImpl
        , downloadJson: \_ -> pureA $ unsafeParse """{ "data": { "image_mp4_url": "_VIDEO_URL_" } }"""
        , telegram:
            { sendVideo: \x -> (unsafeToJson x >>= (\x -> Q.push x log)) *> pure 0 # liftEffect
            , editVideo: \_ _ _ _ -> notImpl
            , updateKeyboard: \_ _ _ -> notImpl } }

  launchAff_ (D.update env msg)

  logA <- Q.toArray log
  assertEqual 
    { expected: [ "{\"chat\":-1001130908027,\"url\":\"_VIDEO_URL_\",\"caption\":\"@no_name, докажите что вы человек.\\nНапишите что происходит на картинке. У вас 30 секунд 😸\",\"keyboard\":[]}" ]
    , actual: logA }

pureA :: ∀ a. a -> Aff a
pureA x = pure x
unsafeParse x = jsonParser x # unsafePartial fromRight
notImpl = throw "not implemented" # liftEffect
