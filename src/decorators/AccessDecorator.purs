module AccessDecorator (makeAccessDecorate) where

import Prelude

import Common (BotMessage, newRef, readRef, writeRef)
import Common as C
import Data.DateTime (DateTime, diff)
import Data.Map (Map, insert, lookup, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Time.Duration (Seconds)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import PureDomain as PD

type State = { lastResetTime ∷ DateTime , users ∷ Map Int Int }

makeEmptyState :: State
makeEmptyState = { lastResetTime : bottom, users : mempty }

makeAccessDecorate :: _ -> Aff (_ -> Aff _)
makeAccessDecorate sendMessage = do
  stateRef <- newRef makeEmptyState

  pure \(msg :: BotMessage) -> do
    now <- nowDateTime # liftEffect
    state <- readRef stateRef
    let duration = diff now state.lastResetTime :: Seconds

    let userId = msg.from.id :: Int

    if diff now state.lastResetTime > PD.limitPerSedonds
      then do
        writeRef stateRef { lastResetTime : now, users : singleton userId 1 }
        pure $ Just msg
      else
        let chatType = msg.chat # toMaybe # map (\chat -> chat.type) in
        case C.tryExtractCommand msg of
          Nothing -> pure $ Just msg
          Just _ ->
            let isSupergroup = chatType == (pure "supergroup") in
            let counts = lookup userId state.users # fromMaybe 0 in
            if counts >= PD.limitCount
              then do
                _ <-
                  sendMessage
                    { chatId: msg.chat # toMaybe # map (\chat -> chat.id) # fromMaybe ""
                    , text: "Хватит абьюзить бота (лимит: " <> (show PD.limitCount) <> " картинки в " <> (show PD.limitPerSedonds) <> ")"
                    , reply_message_id : msg.message_id
                    }
                pure Nothing
              else do
                writeRef stateRef (state { users = insert userId (counts + 1) state.users })
                pure $ Just msg
