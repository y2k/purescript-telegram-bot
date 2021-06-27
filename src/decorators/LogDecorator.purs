module LogDecorator (logDecorate) where

import Prelude
import Effect.Class.Console (log)

logDecorate next msg = do
  log $ "[LOG] Message from: " <> msg.from.first_name <> " (" <> (show msg.from.id) <> "), text = " <> (show msg.text)
  next msg
