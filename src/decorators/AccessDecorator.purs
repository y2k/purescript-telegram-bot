module AccessDecorator (makeAccessDecorate) where

import Prelude

import Common (newRef, readRef, writeRef)
import Common as C
import Data.DateTime (diff)
import Data.Foldable (sequence_)
import Data.Map (insert, lookup, singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.Time.Duration (Seconds)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2)
import Effect.Now (nowDateTime)
import PureDomain as PD

restrictAccess :: _ -> PD.State -> _ -> _
restrictAccess env state msg =
  let userId = msg.from.id :: Int in
  if diff env.now state.lastResetTime > PD.limitPerSedonds
    then tuple2 [ env.updateState { lastResetTime : env.now, users : singleton userId 1 } ] true
    else
      let chatType = msg.chat # toMaybe # map (\chat -> chat.type) in
      case C.tryExtractCommand msg of
        Nothing -> tuple2 [] true
        Just _ ->
          let isSupergroup = chatType == (pure "supergroup") in
          let counts = lookup userId state.users # fromMaybe 0 in
          if counts >= PD.limitCount
            then tuple2 [ env.reply $ "Хватит абьюзить бота (лимит: " <> (show PD.limitCount) <> " картинки в " <> (show PD.limitPerSedonds) <> ")" ] false
            else tuple2 [ env.updateState (state { users = insert userId (counts + 1) state.users }) ] true

makeAccessDecorate sendMessage =
  let accessDecorate state next msg = do
        now <- nowDateTime
        currentState <- readRef state

        let duration = diff now currentState.lastResetTime :: Seconds

        let (Tuple effs (Tuple allowNext _)) =
              restrictAccess
                { reply : (\text -> do
                    _ <- sendMessage
                            { chatId : msg.chat # toMaybe # map (\chat -> chat.id) # fromMaybe ""
                            , text : text
                            , reply_message_id : msg.message_id
                            }
                    pure unit)
                , now
                , updateState : (writeRef state) }
                currentState
                msg
        sequence_ effs
        if allowNext
          then next msg
          else pure unit in
  do
    state <- newRef PD.makeEmptyState
    pure $ accessDecorate state
