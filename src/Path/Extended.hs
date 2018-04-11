{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , NamedFieldPuns
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module Path.Extended
  ( -- * Types
    Location (..)
  , QueryParam
  , -- * Classes
    ToPath (..)
  , ToLocation (..)
  , FromPath (..)
  , FromLocation (..)
  , -- * Combinators
    -- ** Append
    PathAppend (..)
  , -- ** Parent Accessors
    addParent
  , delParent
  , -- ** Path
    fromPath
  , -- ** File Extensions
    setFileExt
  , addFileExt
  , delFileExt
  , getFileExt
  , -- ** Query Parameters
    setQuery
  , addQuery
  , (<&>)
  , addQueries
  , delQuery
  , getQuery
  , -- ** Fragment
    setFragment
  , addFragment
  , (<#>)
  , delFragment
  , getFragment
  , -- ** Parsers
    locationAbsDirParser
  , locationAbsFileParser
  ) where

import Path as P hiding ((</>))
import qualified Path as P ((</>))

import Prelude hiding (takeWhile)
import Data.List (intercalate)
import Data.Attoparsec.Text (Parser, char, takeWhile, takeWhile1, sepBy, sepBy1)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Applicative (Alternative (many), optional)
import Control.Exception (SomeException)
import Control.Monad (void)


class PathAppend right base type' where
  (</>) :: Path base Dir -> right Rel type' -> right base type'


-- | Convenience typeclass for symbolic, stringless routes - make an instance
-- for your own data type to use your constructors as route-referencing symbols.
class ToPath sym base type' | sym -> base type' where
  toPath :: sym -> Path base type'

-- | Convenience typeclass for symbolic, stringless routes - make an instance
-- for your own data type to use your constructors as route-referencing symbols.
class ToLocation sym base type' | sym -> base type' where
  toLocation :: sym -> Location base type'

class FromPath sym base type' | sym -> base type' where
  parsePath :: Path base type' -> Either String sym

class FromLocation sym base type' | sym -> base type' where
  parseLocation :: Location base type' -> Either String sym



-- | A location for some base and type - internally uses @Path@.
data Location b t = Location
  { locParentJumps :: Int -- ^ only when b ~ Rel
  , locPath        :: Path b t
  , locFileExt     :: Maybe String -- ^ only when t ~ File
  , locQueryParams :: [QueryParam]
  , locFragment    :: Maybe String
  } deriving (Eq, Ord)




locationAbsDirParser :: Parser (Location Abs Dir)
locationAbsDirParser = do
  locPath <- absDir
  locQueryParams <- do
    xs <- optional $ do
      let val = T.unpack <$> takeWhile (`notElem` ['=','&','#'])
      void (char '?')
      let kv = do
            k <- val
            mV <- optional $ do
              void (char '=')
              val
            pure (k,mV)
      kv `sepBy` void (char '&')
    case xs of
      Nothing -> pure []
      Just xs' -> pure xs'
  locFragment <- optional $ do
    void (char '#')
    xs <- takeWhile (const True)
    pure (T.unpack xs)
  pure Location
    { locParentJumps = 0
    , locPath
    , locFileExt = Nothing
    , locQueryParams
    , locFragment
    }
  where
    divider = void (char '/')
    chunk = takeWhile1 (`notElem` ['?','&','/','#'])
    absDir :: Parser (Path Abs Dir)
    absDir = do
      divider
      xs <- many (chunk <* divider)
      case parseAbsDir ( case xs of
                           [] -> "/"
                           _  -> T.unpack ("/" <> T.intercalate "/" xs <> "/")
                       ) of
        Left (e :: SomeException) -> fail (show e)
        Right x -> pure x


locationAbsFileParser :: Parser (Location Abs File)
locationAbsFileParser = do
  (locPath, locFileExt) <- absFile
  locQueryParams <- do
    xs <- optional $ do
      let val = T.unpack <$> takeWhile (`notElem` ['=','&','#'])
      void (char '?')
      let kv = do
            k <- val
            mV <- optional $ do
              void (char '=')
              val
            pure (k,mV)
      kv `sepBy` void (char '&')
    case xs of
      Nothing -> pure []
      Just xs' -> pure xs'
  locFragment <- optional $ do
    void (char '#')
    xs <- takeWhile (const True)
    pure (T.unpack xs)
  pure Location
    { locParentJumps = 0
    , locPath
    , locFileExt
    , locQueryParams
    , locFragment
    }
  where
    divider = void (char '/')
    chunk = takeWhile1 (`notElem` ['?','&','/','#'])
    absFile :: Parser (Path Abs File, Maybe String)
    absFile = do
      divider
      xs <- chunk `sepBy1` divider
      let (withoutFE,withFE) = T.breakOn "." (last xs)
          xs' = init xs <> [withoutFE]
      path <- case parseAbsFile (T.unpack ("/" <> T.intercalate "/" xs')) of
        Left (e :: SomeException) -> fail (show e)
        Right x -> pure x
      pure
        ( path
        , if T.null withFE
          then Nothing
          else Just $ T.unpack $ T.drop 1 withFE
        )


instance PathAppend Path Abs Dir where
  (</>) = (P.</>)

instance PathAppend Path Abs File where
  (</>) = (P.</>)

instance PathAppend Path Rel Dir where
  (</>) = (P.</>)

instance PathAppend Path Rel File where
  (</>) = (P.</>)

instance PathAppend Location Abs Dir where
  l </> (Location ps pa fe qp fr) =
    Location ps ((P.</>) l pa) fe qp fr

instance PathAppend Location Abs File where
  l </> (Location ps pa fe qp fr) =
    Location ps ((P.</>) l pa) fe qp fr

instance PathAppend Location Rel Dir where
  l </> (Location ps pa fe qp fr) =
    Location ps ((P.</>) l pa) fe qp fr

instance PathAppend Location Rel File where
  l </> (Location ps pa fe qp fr) =
    Location ps ((P.</>) l pa) fe qp fr


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

getFragment :: Location b t -> Maybe String
getFragment (Location _ _ _ _ x) = x
