module LogDecorator (logDecorate) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)

logDecorate msg = do
  log $ "[LOG] Message from: " <> msg.from.first_name <> " (" <> (show msg.from.id) <> "), text = " <> (show msg.text)
  pure $ Just msg

-- logDecorate next msg = do
--   log $ "[LOG] Message from: " <> msg.from.first_name <> " (" <> (show msg.from.id) <> "), text = " <> (show msg.text)
--   next msg
