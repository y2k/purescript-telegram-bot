module Domain (update, restrictAccess) where

import Prelude

import Common as C
import Data.DateTime (diff)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (notNull, null, toMaybe)
import Data.Nullable as Nullable
import Data.Time.Duration (Seconds(..), fromDuration)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2, tuple3)
import Effect.Aff (Aff)
import PureDomain as D

restrictAccess :: _ -> D.State -> _ -> _
restrictAccess env state msg =
  let userId = msg.from.id :: Int in
  if diff env.now state.lastResetTime > D.limitPerSedonds
    then tuple2 [ env.updateState { lastResetTime : env.now, users : Map.singleton userId 1 } ] true
    else
      let chatType =
            msg.chat
            # Nullable.toMaybe
            # map (\chat -> chat.type) in
      case C.tryExtractCommand msg of
        Nothing -> tuple2 [] true
        Just _ ->
          let isSupergroup = chatType == (pure "supergroup") in
          let counts = Map.lookup userId state.users # fromMaybe 0 in
          if counts >= D.limitCount
            then tuple2 [ env.reply $ "Хватить абьюзить бота (лимит: " <> (show D.limitCount) <> " картинки в " <> (show D.limitPerSedonds) <> ")" ] false
            else tuple2 [ env.updateState (state { users = Map.insert userId (counts + 1) state.users }) ] true

update :: _ -> _ -> Aff Unit
update env msg = do
  _ <- updateHandleCommand env msg
  _ <- updateHandleReroll env msg
  _ <- updateHandleLogin env msg
  pure unit

updateHandleCommand :: _ -> _ -> Aff Unit
updateHandleCommand env msg =
  case C.tryExtractCommand msg of
    Nothing -> pure unit
    Just text -> handleCommand env msg text

updateHandleReroll :: _ -> _ -> Aff Unit
updateHandleReroll env msg =
  case C.tryExtractCommand msg of
    Just text -> pure unit
    Nothing ->
      case toMaybe msg.message of
        Just message ->
          case toMaybe msg.data of
            Nothing -> pure unit
            Just data' -> do
              case C.unpackData' data' of
                [ "reroll", tag ] -> handleReroll env tag message
                _ -> pure unit
        Nothing ->
          pure unit

updateHandleLogin :: _ -> _ -> Aff Unit
updateHandleLogin env msg =
  case C.tryExtractCommand msg of
    Just text -> pure unit
    Nothing ->
      case toMaybe msg.message of
        Just message -> pure unit
        Nothing ->
          case tuple3 (toMaybe msg.chat) (toMaybe msg.message_id) (toMaybe msg.new_chat_member) of
            Tuple (Just chat) (Tuple (Just message_id) (Tuple (Just newChatMember) unit)) ->
              handleLogin env chat message_id newChatMember
            _ -> pure unit

handleCommand env msg text =
  case text of
    [ "cat" ] -> sendVideo env msg "cat"
    [ "dog" ] -> sendVideo env msg "puppy"
    [ "debug_get_user_id" ] -> getUserId env msg
    [ "debug_reply", userId ] -> testMention env msg userId
    _ -> pure unit

getUserId env msg = do
  chat <- C.unwrapNullable msg.chat
  reply <- C.unwrapNullable msg.reply_to_message
  let userId = reply.from.id :: Int
  response <- env.telegram.sendMessage
                { chatId: chat.id
                , text: "UserID: " <> (show userId)
                , reply_message_id: null }
  _ <- env.delay $ fromDuration $ Seconds 5.0
  _ <- env.telegram.deleteMessage { chat: chat.id, message_id: response.message_id }
  pure unit

testMention env msg userId = do
  chat <- C.unwrapNullable msg.chat
  _ <- env.delay $ fromDuration $ Seconds 5.0
  _ <- env.telegram.sendVideo
        { chat_id: chat.id
        , reply_to_message_id: null
        , url: "https://i.giphy.com/media/fT3PPZwB2lZMk/giphy.gif"
        , caption: notNull $ "Привет [user_name](tg://user?id=" <> userId <> "), оптишись плиз, была ли нотификация"
        , keyboard: [] }
  pure unit

handleLogin env chat message_id newChatMember = do
  let username =
        toMaybe newChatMember.username
        # maybe newChatMember.first_name (\username -> "@" <> username)

  json <- D.makeUrl env.token "cat" # env.downloadJson
  info <- D.parseImageJson json # C.unwrapEither

  let timeout = 30

  response <-
    env.telegram.sendVideo
      { chat_id: chat.id
      , reply_to_message_id: notNull message_id
      , url: info.data.image_mp4_url
      , caption: username <> ", докажите что вы человек.\nНапишите что происходит на картинке. У вас " <> (show timeout) <> " секунд 😸" # notNull
      , keyboard: [] }
  _ <- env.delay $ fromDuration $ Seconds $ toNumber timeout
  _ <- env.telegram.deleteMessage { chat: chat.id, message_id: response.message_id }
  pure unit

handleReroll env tag message = do
  json <- D.makeUrl env.token tag # env.downloadJson
  info <- D.parseImageJson json # C.unwrapEither

  D.makeRerollVideoRequest info tag message # env.telegram.editMessageMedia

sendVideo env msg tag = do
  chat <- C.unwrapNullable msg.chat
  json <- D.makeUrl env.token tag # env.downloadJson
  info <- D.parseImageJson json # C.unwrapEither

  { message_id } <-
    D.sendVideoWithRerollKeyboard chat info tag
    # env.telegram.sendVideo

  _ <- env.delay $ fromDuration $ Seconds 15.0

  env.telegram.editMessageReplyMarkup { chat_id: chat.id, message_id, keyboard: [] }
