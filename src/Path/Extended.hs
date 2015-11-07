module Path.Extended
  ( -- * Types
    Location
  , QueryParam
    -- * Combinators
    -- ** Parent Accessors
  , addParent
  , delParent
    -- ** Path
  , fromPath
    -- ** File Extensions
  , setFileExt
  , addFileExt
  , delFileExt
  , getFileExt
    -- ** Query Parameters
  , setQuery
  , addQuery
  , (<&>)
  , addQueries
  , delQuery
  , getQuery
    -- ** Fragment
  , setFragment
  , addFragment
  , (<#>)
  , delFragment
  , module P
  ) where

import Path as P
import Data.List (intercalate)


-- | A location for some base and type - internally uses @Path@.
data Location b t = Location
  { locParentJumps :: Int -- ^ only when b ~ Rel
  , locPath        :: Path b t
  , locFileExt     :: Maybe String -- ^ only when t ~ File
  , locQueryParams :: [QueryParam]
  , locFragment    :: Maybe String
  } deriving (Eq, Ord)

instance Show (Location b t) where
  show (Location js pa fe qp fr) =
    let loc = concat (replicate js "../")
           ++ toFilePath pa
           ++ maybe "" (\f -> "." ++ f) fe
        query = case qp of
                  [] -> ""
                  qs -> "?" ++ intercalate "&" (map go qs)
          where
            go (k,mv) = k ++ maybe "" (\v -> "=" ++ v) mv
    in loc ++ query ++ maybe "" (\f -> "#" ++ f) fr

type QueryParam = (String, Maybe String)



-- | Prepend a parental accessor path - @../@
addParent :: Location Rel t -> Location Rel t
addParent (Location j pa fe qp fr) =
  Location (j+1) pa fe qp fr

delParent :: Location Rel t -> Location Rel t
delParent l@(Location j pa fe qp fr)
  | j <= 0    = l
  | otherwise = Location (j-1) pa fe qp fr


-- | This should be your entry point for creating a @Location@.
fromPath :: Path b t -> Location b t
fromPath pa = Location 0 pa Nothing [] Nothing


setFileExt :: Maybe String -> Location b File -> Location b File
setFileExt fe (Location js pa _ qp fr) =
  Location js pa fe qp fr

addFileExt :: String -> Location b File -> Location b File
addFileExt fe = setFileExt (Just fe)

delFileExt :: Location b File -> Location b File
delFileExt = setFileExt Nothing

getFileExt :: Location b File -> Maybe String
getFileExt (Location _ _ fe _ _) =
  fe


setQuery :: [QueryParam] -> Location b t -> Location b t
setQuery qp (Location js pa fe _ fr) =
  Location js pa fe qp fr

-- | Appends a query parameter
addQuery :: QueryParam -> Location b t -> Location b t
addQuery q (Location js pa fe qp fr) =
  Location js pa fe (qp ++ [q]) fr

(<&>) :: Location b t -> QueryParam -> Location b t
(<&>) = flip addQuery

infixl 7 <&>

addQueries :: [QueryParam] -> Location b t -> Location b t
addQueries qs (Location js pa fe qs' fr) =
  Location js pa fe (qs' ++ qs) fr

delQuery :: Location b t -> Location b t
delQuery = setQuery []

getQuery :: Location b t -> [QueryParam]
getQuery (Location _ _ _ qp _) =
  qp


setFragment :: Maybe String -> Location b t -> Location b t
setFragment fr (Location js pa fe qp _) =
  Location js pa fe qp fr

addFragment :: String -> Location b t -> Location b t
addFragment fr = setFragment (Just fr)

(<#>) :: Location b t -> String -> Location b t
(<#>) = flip addFragment

infixl 8 <#>

delFragment :: Location b t -> Location b t
delFragment = setFragment Nothing

