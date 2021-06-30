module HandleMessageDecorator (makeHanleMessageUpdate) where

import Common (bindBotPart)
import LoginHandler (handleLogin)
import PostGifHandler (handlePostGif)
import RerollHandler (handleReroll)

makeHanleMessageUpdate env =
  bindBotPart
    (bindBotPart
      (handleReroll env)
      (handleLogin env))
    (handlePostGif env)
