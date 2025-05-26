{-# LANGUAGE OverloadedStrings #-}

module Main where

import Types
import Auth 

import Control.Monad.Except
import Control.Monad.Reader
import System.Exit (exitSuccess)

main :: IO ()
main = do
  configResult <- loadConfig "config.json"
  case configResult of
    Left err -> putStrLn $ "Failed to load configuration: " ++ err
    Right config -> loop (maxAttempts config) config

loop :: Int -> Config -> IO ()
loop 0 _ = do
  putStrLn "No attempts left. Exiting."
  exitSuccess
  
loop attemptsLeft config = do
  putStrLn $ "Attempts left: " ++ show attemptsLeft
  putStrLn "Enter token:"
  token <- getLine
  putStrLn "Do you want access to the admin panel? (yes/no)"
  adminInput <- getLine
  let wantsAdmin = adminInput == "yes"
  result <- runExceptT $ runReaderT (checkAccess token wantsAdmin attemptsLeft) config
  case result of
    Right _ -> putStrLn "Access granted."
    Left err -> do
      putStrLn $ "Error: " ++ show err
      case err of
        NoAttemptsLeft -> exitSuccess
        InvalidToken   -> loop (attemptsLeft - 1) config
        ForbiddenResource -> loop attemptsLeft config      

defcfg :: Config
defcfg = Config {
  adminToken = "secret 123",
  userTokens = ["abc", "def"],
  maxAttempts = 3
}