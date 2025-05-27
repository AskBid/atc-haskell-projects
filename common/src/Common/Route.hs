{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
  FrontendRoute_Login  :: FrontendRoute ()
  FrontendRoute_Signup :: FrontendRoute ()
  FrontendRoute_Profile :: FrontendRoute Text

-- | mkFullRouteEncoder is a helper function provided by Obelisk to create an Encoder that converts between:
-- Obelisk uses a type-safe routing system: instead of using raw strings for URLs everywhere, 
-- you define Haskell data types representing your routes, e.g.:
-- But your web browser uses URLs like /login or /profile/alice.
-- mkFullRouteEncoder connects these two worlds:
--  - When your app receives a URL, it parses it into one of your route types.
--  - When your app wants to generate a link, it encodes a route type back into a URL.
fullRouteEncoder :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ()) 
  -- ^ 404 handler
  (\case
    BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty)
  (\case
    FrontendRoute_Main -> PathEnd $ unitEncoder mempty
    FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
    FrontendRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
    FrontendRoute_Profile -> PathSegment "profile" $ singlePathSegmentEncoder 
  )

-- unitEncoder is for static routes (no parameters).
-- segmentEncoder is for routes with one dynamic segment (like a Text value).

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
