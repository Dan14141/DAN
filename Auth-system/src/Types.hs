{-# LANGUAGE OverloadedStrings , DeriveGeneric #-}

module Types (loadConfig, Config(..), AuthError(..)) where

import qualified Data.ByteString.Lazy as Byte
import Data.Aeson
import GHC.Generics

data Config = Config
  { adminToken  :: String
  , userTokens  :: [String]
  , maxAttempts :: Int
  } deriving (Show, Generic)

data AuthError = InvalidToken
  | NoAttemptsLeft
  | ForbiddenResource
  deriving (Show, Eq)

instance FromJSON Config
loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = eitherDecode <$> Byte.readFile path
