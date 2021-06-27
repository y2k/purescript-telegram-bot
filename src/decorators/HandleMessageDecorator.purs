module HandleMessageDecorator where

import Prelude

import Affjax.ResponseFormat (json)
import Control.Monad.Error.Class (try)
import Control.Promise (toAffE)
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import LoginHandler (handleLogin)
import PostGifHandler (handlePostGif)
import RerollHandler (handleReroll)

handleUpdate env msg =
  [ handleReroll
  , handleLogin
  , handlePostGif
  ]
  <#> (\f -> f env msg)
  <#> try # sequence # void

makeHandleMessageDecorator sendMessage deleteMessage sendVideo editMessageMedia editMessageReplyMarkup download delay apiKey nowDateTime msg = launchAff_ $ do
  nowTime <- liftEffect nowDateTime

  result <- handleUpdate
              { token: apiKey
              , delay: delay
              , downloadJson: download json
              , telegram:
                  { editMessageReplyMarkup: (\p -> editMessageReplyMarkup p *> pure unit # liftEffect)
                  , editMessageMedia: (\p -> editMessageMedia p *> pure unit # liftEffect)
                  , sendVideo: (\p -> sendVideo p # toAffE)
                  , deleteMessage: (\p -> deleteMessage p *> pure unit # liftEffect)
                  , sendMessage: (\p -> sendMessage p # toAffE) } }
              msg
            # try
  case result of
    Right _ -> pure unit
    Left e -> log $ "[LOG][ERROR] " <> show e
