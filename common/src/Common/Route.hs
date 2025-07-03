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
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE DerivingStrategies    #-}

module Common.Route where

-- {- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-- -}

import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

import Control.Lens.Combinators (makeWrapped)
import Control.Monad.Error
import Data.Universe

  -- | Used to handle unparseable routes.
data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute Api
  -- BackendRoute_Logout :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data Api
  = Api_Login
  | Api_Logout
  | Api_Me
  | Api_Posts
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe Api

tailRouteEncoder 
  :: (MonadError Text parse, MonadError Text check) 
  => Encoder check parse Api PageName
tailRouteEncoder = enumEncoder $ \case
  Api_Login  -> (["login"], mempty)
  Api_Logout -> (["logout"], mempty)
  Api_Me     -> (["me"], mempty)
  Api_Posts  -> (["posts"], mempty)

-- newtype UserID = UserID { unUserID :: Text } deriving (Show, Eq)
-- makeWrapped ''UserID

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
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
  (FullRoute_Backend BackendRoute_Missing :/ ()) -- ^ 404 handler (first arg only, second is below)
  (\case
    BackendRoute_Api -> PathSegment "api" tailRouteEncoder
    BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
  )
  (\case
    FrontendRoute_Main -> PathEnd $ unitEncoder mempty
    FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
    FrontendRoute_Signup -> PathSegment "signup" $ unitEncoder mempty
    FrontendRoute_Profile -> PathSegment "profile" $ singlePathSegmentEncoder
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]

-- https://github.com/obsidiansystems/obelisk/blob/master/lib/route/docs/introduction.md#add-parameter-to-a-nested-route

-- unitEncoder is for static routes (no parameters).
-- segmentEncoder is for routes with one dynamic segment (like a Text value).

-- enumEncoder
--   :: (Universe p, Ord p, Ord r, Text parse, Text check, Show p, Show r) 
--   => (p -> r) -> Encoder check parse p r

-- pathComponentEncoder
--   :: (UniverseSome p, GShow p, GCompare p, MonadError Text check, MonadError Text parse) 
--   => (p a -> SegmentResult check parse a) -> Encoder check parse (R p) PageName
--
-- Notice that enumEncoder takes a function in which we provide a type `p` and return a PageName `r`
-- that's non other that the `\case` function we dispatch the different routes with.
-- PageName is a URL reprensentation, not a class type, that's why it doesnt appear in the constriants
-- but the contraint it needs is to be of type Show as the URL representation obivously needs that.
-- we use `r` just for reusability, as could imagine a completely different use of enumEncoder, like 
-- encoding to JSON keys, or CLI argument strings, or some intermediate data format.
--
-- In `pathComponenetEncoder` we take instead a parametrized type `p a` and return a `SegmentResult` 
-- rather than a URL representation, under the flexible variable `r` usually `PageName`.

-- data SegmentResult check parse a
--   = PathEnd (Encoder check parse a (Map Text (Maybe Text)))
--   | PathSegment Text (Encoder check parse a PageName)
--
--   :t PathEnd 
--   PathEnd :: Encoder check parse a (Map Text (Maybe Text)) -> SegmentResult check parse a
--   :t PathSegment
--   PathSegment :: Text -> Encoder check parse a PageName -> SegmentResult check parse a

-- the reason for SegmentResult is that routes may be nested in the definition of other routes. 
-- We need a way to describe whether a route may have more segments or if we've reached the end.
-- That's why the SegmentResult type has two constructor types.
-- .../pull   <-- that's a page            ../pull :: PathEnd
-- .../pull/6 <-- another page             ../pull :: PathSegment
-- .../pull/9 <-- but there can be more    ../pull :: PathSegment
-- .../pull/3 <-- abut there can be more   ../pull :: PathSegment
--
-- The SegmentResult has two constructors, one recursive allowing for more structure, 
-- and the other terminating, indicating that this path is complete.
