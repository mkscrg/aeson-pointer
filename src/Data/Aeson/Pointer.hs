module Data.Aeson.Pointer
  ( Pointer
  , fromList
  , toList
  , index
  ) where

import Control.Monad
import Data.String

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V


newtype Pointer = Pointer [Text]
    deriving (Eq, Read, Show)

instance FromJSON Pointer where
    parseJSON = withText "Pointer" $ pointerFromString . T.unpack

instance ToJSON Pointer where
    toJSON (Pointer refs) = String $ T.concat $ map escape refs
      where
        escape = T.cons '/' . T.concatMap escape'
          where
            escape' '~' = "~0"
            escape' '/' = "~1"
            escape' c = T.singleton c

instance IsString Pointer where
    fromString s = case parse pointerFromString s of
        Success ptr -> ptr
        Error msg -> error msg


fromList :: [Text] -> Pointer
fromList = Pointer

toList :: Pointer -> [Text]
toList (Pointer refs) = refs

index :: FromJSON a => Value -> Pointer -> Parser a
index val (Pointer refs) = foldM go val refs >>= parseJSON
  where
    go (Object o) ref = o .: ref
    go (Array a) ref = readP (T.unpack ref) >>= \i -> case a V.!? i of
        Nothing -> fail "Tried to index Array with out-of-bounds ref"
        Just v -> return v
    go _ _ = fail "Tried to index non-container node"
    -- ripped off 'readMay' from 'safe': http://hackage.haskell.org/package/safe-0.3.3/docs/src/Safe.html#readMay
    readP s = case [x | (x,t) <- reads s, ("","") <- lex t] of
        [x] -> return x
        _ -> fail "Tried to index Array with non-Int ref"


pointerFromString :: String -> Parser Pointer
pointerFromString = fmap (Pointer . map (T.pack . unescape)) . split
  where
    split ('/':cs) = case break (== '/') cs of
        (r, cs') -> fmap (r :) $ split cs'
    split [] = return []
    split _ = fail "Pointer must start with '/'"
    unescape ('~':'0':cs) = '~' : unescape cs
    unescape ('~':'1':cs) = '/' : unescape cs
    unescape (c:cs) = c : unescape cs
    unescape [] = []
