{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

import Data.Map.Strict (Map)

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- BackendRoute_WebsocketUser :: BackendRoute Text
  BackendRoute_Websocket :: BackendRoute (R WebsocketRoute)
  BackendRoute_Login :: BackendRoute ()
  BackendRoute_Signup :: BackendRoute ()
  BackendRoute_Logout :: BackendRoute ()
  BackendRoute_Me :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- FrontendRoute_Signup :: FrontendRoute ()
  FrontendRoute_User :: FrontendRoute Text
  -- This type is used to define frontend routes, i.e. ones for which 
  -- the backend will serve the frontend.

data WebsocketRoute :: * -> * where
  WebscocketRoute_Main :: WebsocketRoute () 
  WebscocketRoute_User :: WebsocketRoute Text 

websocketRouteEncoder :: Encoder (Either Text) (Either Text) (R WebsocketRoute) PageName
websocketRouteEncoder = pathComponentEncoder $ \case
  WebscocketRoute_Main -> PathEnd $ unitEncoder mempty
  WebscocketRoute_User -> PathSegment "user" $ singlePathSegmentEncoder

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      BackendRoute_Signup -> PathSegment "signup-request" $ unitEncoder mempty
      BackendRoute_Logout -> PathSegment "logout" $ unitEncoder mempty
      BackendRoute_Me -> PathSegment "me" $ unitEncoder mempty
      BackendRoute_Websocket -> PathSegment "ws" $ websocketRouteEncoder
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      -- FrontendRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
      FrontendRoute_User -> PathSegment "user" $ singlePathSegmentEncoder
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''WebsocketRoute
  ]
