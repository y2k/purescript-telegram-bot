module HandleMessageDecorator where

import Prelude

import Affjax.ResponseFormat (json)
import Common (logDecorate)
import Control.Monad.Error.Class (try)
import Control.Promise (toAffE)
import Data.Either (Either(..))
import Data.Traversable (for_, sequence)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (message)
import LoginHandler (handleLogin)
import PostGifHandler (handlePostGif)
import RerollHandler (handleReroll)

handleUpdate env msg = do
  results <-
    [ handleReroll
    , handleLogin
    , handlePostGif
    ]
    <#> (\f -> f env msg)
    <#> try # sequence

  for_ results (\n ->
    case n of
      Left e | message e /= "no_value" ->
        log $ "[ERROR][handleUpdate] " <> (show e)
      _ -> pure unit)

makeHandleMessageDecorator sendMessage deleteMessage sendVideo editMessageMedia editMessageReplyMarkup download delay apiKey nowDateTime msg = launchAff_ $ do
  nowTime <- liftEffect nowDateTime

  result <- handleUpdate
              { token: apiKey
              , delay: delay
              , downloadJson: download json
              , telegram:
                  { editMessageReplyMarkup: logDecorate "editMessageReplyMarkup" (\p -> editMessageReplyMarkup p *> pure unit # liftEffect)
                  , editMessageMedia: logDecorate "editMessageMedia" (\p -> editMessageMedia p *> pure unit # liftEffect)
                  , sendVideo: logDecorate "sendVideo" (\p -> sendVideo p # toAffE)
                  , deleteMessage: logDecorate "deleteMessage" (\p -> deleteMessage p *> pure unit # liftEffect)
                  , sendMessage: logDecorate "sendMessage" (\p -> sendMessage p # toAffE) } }
              msg
            # try
  case result of
    Right _ -> pure unit
    Left e -> log $ "[LOG][ERROR] " <> show e
