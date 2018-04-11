{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , NamedFieldPuns
  , ScopedTypeVariables
  , OverloadedStrings
  , QuasiQuotes
  , DeriveGeneric
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
    -- ** Path
    fromAbsDir
  , fromAbsFile
  , prepend
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
  , -- ** Parser & Printer
    locationParser
  , printLocation
  ) where

-- import Path as P hiding ((</>))
import Path (Path, Abs, Dir, File, (</>), toFilePath, parseAbsFile, parseAbsDir, stripProperPrefix, absdir)

import Prelude hiding (takeWhile)
import Data.List (intercalate)
import Data.Attoparsec.Text (Parser, char, takeWhile, takeWhile1, sepBy, sepBy1, many1)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Control.Applicative (Alternative (many), (<|>), optional)
import Control.Exception (SomeException)
import Control.Monad (void)
import GHC.Generics (Generic)



-- | Convenience typeclass for symbolic, stringless routes - make an instance
-- for your own data type to use your constructors as route-referencing symbols.
class ToPath sym base type' | sym -> base type' where
  toPath :: sym -> Path base type'

-- | Convenience typeclass for symbolic, stringless routes - make an instance
-- for your own data type to use your constructors as route-referencing symbols.
class ToLocation sym where
  toLocation :: sym -> Location

class FromPath sym base type' | sym -> base type' where
  parsePath :: Path base type' -> Either String sym

class FromLocation sym where
  parseLocation :: Location -> Either String sym



-- | A location for some base and type - internally uses @Path@.
data Location = Location
  { locPath        :: Either (Path Abs Dir) (Path Abs File)
  , locQueryParams :: [QueryParam]
  , locFragment    :: Maybe String
  } deriving (Eq, Ord, Generic)



fromAbsDir :: Path Abs Dir -> Location
fromAbsDir path = Location
  { locPath = Left path
  , locQueryParams = []
  , locFragment = Nothing
  }

fromAbsFile :: Path Abs File -> Location
fromAbsFile path = Location
  { locPath = Right path
  , locQueryParams = []
  , locFragment = Nothing
  }


locationParser :: Parser Location
locationParser = do
  divider
  locPath <- do
    xs <- chunk `sepBy` divider
    case xs of
      [] -> pure (Left [absdir|/|])
      _ -> do
        let dir = do
              divider
              case parseAbsDir (T.unpack ("/" <> T.intercalate "/" xs <> "/")) of
                Left (e :: SomeException) -> fail (show e)
                Right x -> pure (Left x)
            file =
              case parseAbsFile (T.unpack ("/" <> T.intercalate "/" xs)) of
                Left (e :: SomeException) -> fail (show e)
                Right x -> pure (Right x)
        dir <|> file
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
    { locPath
    , locQueryParams
    , locFragment
    }
  where
    divider = void (char '/')
    chunk = takeWhile1 (`notElem` ['?','&','/','#'])


prepend :: Path Abs Dir -> Location -> Location
prepend path l@Location{locPath} = l
  { locPath = case locPath of
      Left d -> case stripProperPrefix [absdir|/|] d of
        Nothing -> error "impossible state"
        Just d' -> Left (path </> d')
      Right f -> case stripProperPrefix [absdir|/|] f of
        Nothing -> error "impossible state"
        Just f' -> Right (path </> f')
  }



printLocation :: Location -> T.Text
printLocation (Location pa qp fr) =
  let loc = either toFilePath toFilePath pa
      query = case qp of
                [] -> ""
                qs -> "?" <> T.intercalate "&" (go <$> qs)
        where
          go (k,mv) = T.pack k <> maybe "" (\v -> "=" <> T.pack v) mv
  in T.pack loc <> query <> maybe "" (\f -> "#" <> T.pack f) fr

type QueryParam = (String, Maybe String)



setQuery :: [QueryParam] -> Location -> Location
setQuery qp (Location pa _ fr) =
  Location pa qp fr

-- | Appends a query parameter
addQuery :: QueryParam -> Location -> Location
addQuery q (Location pa qp fr) =
  Location pa (qp ++ [q]) fr

(<&>) :: Location -> QueryParam -> Location
(<&>) = flip addQuery

infixl 7 <&>

addQueries :: [QueryParam] -> Location -> Location
addQueries qs (Location pa qs' fr) =
  Location pa (qs' ++ qs) fr

delQuery :: Location -> Location
delQuery = setQuery []

getQuery :: Location -> [QueryParam]
getQuery (Location _ qp _) =
  qp


setFragment :: Maybe String -> Location -> Location
setFragment fr (Location pa qp _) =
  Location pa qp fr

addFragment :: String -> Location -> Location
addFragment fr = setFragment (Just fr)

(<#>) :: Location -> String -> Location
(<#>) = flip addFragment

infixl 8 <#>

delFragment :: Location -> Location
delFragment = setFragment Nothing

getFragment :: Location -> Maybe String
getFragment (Location _ _ x) = x
