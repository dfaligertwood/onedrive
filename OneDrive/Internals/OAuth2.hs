{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module OneDrive.Internals.OAuth2
  ( Auth
  , getAuth
  , newAuth
  , signIn
  , offline
  , readOnly
  , readWrite
  , appFolder
  ) where
--------------------------------------------------------------------------------

import qualified Data.ByteString as B
import           Data.Default

--------------------------------------------------------------------------------

data Client = Client
  { clientId      :: B.ByteString
  , clientSecret  :: B.ByteString
  , redirectURI   :: String
  , clientScope   :: Scope
  }

data Token = Token
  { accessToken  :: B.ByteString
  , tokenType    :: String
  , expiresIn    :: Integer
  , scope        :: Scope
  , refreshToken :: B.ByteString
  }

type AuthCode = B.ByteString
type Scope = [B.ByteString]

data Auth = Auth
  { client    :: Client
  , token     :: Maybe Token
  , authCode  :: Maybe AuthCode
  }

--------------------------------------------------------------------------------

instance Default Client where
  def = Client
    { clientId      = error "Define a client id"
    , clientSecret  = error "Define a client secret"
    , redirectURI   = "https://login.live.com/oauth20_desktop.srf"
    , clientScope   = signIn ++ offline ++ appFolder
    }

--------------------------------------------------------------------------------

getAuth :: Auth -> IO Auth
getAuth (Auth c (Just t) _) = reToken c t
getAuth (Auth c _ (Just a)) = newToken c a
getAuth (Auth c _ _)        = newToken c $ getAuthCode c

newAuth :: Client -> IO Auth
newAuth c
  = getAuth
  $ Auth c Nothing Nothing

reToken :: Client -> Token -> IO Auth
reToken = undefined

newToken :: Client -> AuthCode -> IO Auth
newToken = undefined

getAuthCode :: Client -> AuthCode
getAuthCode = undefined

signIn, offline, readOnly, readWrite, appFolder :: Scope
signIn = ["wl.signin"]
offline = ["wl.offline_access"]
readOnly = ["onedrive.readonly"]
readWrite = ["onedrive.readwrite"]
appFolder = ["onedrive.appfolder"]
