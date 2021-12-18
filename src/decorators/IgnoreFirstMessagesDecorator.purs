module IgnoreFirstMessagesDecorator (mkIgnoreFirstMessagesDecorator) where

import Prelude

import Common (BotMessage, newRef, readRef)
import Data.DateTime (DateTime, diff)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Now (nowDateTime)

type State = { startTime ∷ DateTime }

ignoreFirstMessagesDecorator :: Seconds -> BotMessage -> Aff (Maybe BotMessage)
ignoreFirstMessagesDecorator durationFromStart msg =
  let ignorTime = Seconds 5.0 in
  if durationFromStart > ignorTime
    then pure $ Just msg
    else do
      log $ "[LOG] Ignore '" <> (show msg.text) <> "' because "  <> (show durationFromStart) <> " < " <> (show ignorTime)
      pure $ Nothing

mkIgnoreFirstMessagesDecorator :: Aff (BotMessage -> Aff (Maybe BotMessage))
mkIgnoreFirstMessagesDecorator = do
  startTime <- liftEffect nowDateTime
  stateRef <- newRef $ { startTime: startTime }

  pure \(msg :: BotMessage) -> do
    now <- nowDateTime # liftEffect
    state <- readRef stateRef
    let duration = diff now state.startTime :: Seconds
    ignoreFirstMessagesDecorator duration msg
