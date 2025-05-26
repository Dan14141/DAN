module Auth (checkAccess, AuthApp) where

import Control.Monad.Reader
import Control.Monad.Except
import Types

type AuthApp = ReaderT Config (ExceptT AuthError IO)

checkAccess :: String -> Bool -> Int -> AuthApp ()
checkAccess token wantsAdmin attempts
  | attempts <= 0 = throwError NoAttemptsLeft
  | otherwise = do
      Config adminTok userToks _ <- ask
      if token == adminTok
        then pure ()
        else if token `elem` userToks
               then if wantsAdmin
                      then throwError ForbiddenResource
                      else pure ()
               else throwError InvalidToken
