{-# LANGUAGE OverloadedStrings #-}

module MyJWT where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Web.JWT as JWT
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import Data.Time.Calendar
import Snap

import Database.Schema

jwtSecret :: JWT.EncodeSigner
jwtSecret = JWT.hmacSecret "my-super-secret-key"

-- | returns a Maybe username Text if the JWT was valid (as in signed and with an username)
verifyJWT :: MonadSnap m => m (Maybe T.Text) 
verifyJWT = do 
  maybeCookie <- getCookie "jwt"
  case maybeCookie of
    Nothing -> return Nothing
    Just (Cookie _ valueJWT _ _ _ _ _) -> do 
      let mVerifiedJWT = JWT.decodeAndVerifySignature (JWT.toVerify jwtSecret) $ TE.decodeUtf8 valueJWT
      case mVerifiedJWT of
        Nothing  -> return Nothing
        Just jwt -> do
          case JWT.sub $ JWT.claims jwt of
            Nothing       -> return Nothing
            Just strOrUri -> do 
              let username = JWT.stringOrURIToText strOrUri
              return $ Just username

-- | Using JWT library to create an encoded JWT ByteString, the likes of: 
--  `asxcasas.asdasdasc.aierhuhdf`
createJWT :: User -> BS.ByteString
createJWT (User _ username _) = do
  let expTime = JWT.numericDate 3600
  let claims = JWT.JWTClaimsSet { 
      JWT.iss = Nothing
    , JWT.sub = JWT.stringOrURI username
    , JWT.aud = Nothing
    , JWT.exp = expTime
    , JWT.nbf = Nothing
    , JWT.iat = Nothing
    , JWT.jti = Nothing
    , JWT.unregisteredClaims = mempty
    }
  TE.encodeUtf8 $ JWT.encodeSigned jwtSecret mempty claims

-- | Value (the first argument) is supposed to be a JWT encoded ByteString.
mkJWTCookie :: BS.ByteString -> Cookie
mkJWTCookie valueJWT = Cookie
  { cookieName     = "jwt"
  , cookieValue    = valueJWT
  , cookieExpires  = Nothing
  -- ^ notice this cookie will have Session as expire as the expiration we set
  --   in the JWT is only relatable to the JWT encoding itself in Value. 
  --   It is checked on the backend side.
  , cookieDomain   = Nothing
  , cookiePath     = Just "/"
  , cookieSecure   = False -- True for production
  , cookieHttpOnly = True
  }

-- | Used to substitute any existent "jwt" token so that with a expired date,
--   the browser should automatically logout the user.
cookieLogout :: Cookie
cookieLogout = Cookie { 
      cookieName     = "jwt"
    , cookieValue    = "logout"
    , cookieExpires  = Just $ UTCTime (fromGregorian 2000 1 1) (secondsToDiffTime 0) 
    -- ^ in Browser > Inspect > Application, you will see that if the date here is in
    --   the future the JWT will remain in the cookies, otherwise if in the pat will
    --   disappear.
    , cookieDomain   = Nothing
    , cookiePath     = Just "/"
    , cookieSecure   = False -- True for production
    , cookieHttpOnly = True
    }
