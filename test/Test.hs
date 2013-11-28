module Main where

import Data.String

import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (Success)

import Data.Aeson.Pointer


main :: IO ()
main = defaultMain
    [ testFromListEq
    , testFromListNeq
    , testToListFromList
    , testToJSONFromJSON
    , testIsString
    , testIndex ]


testFromListEq :: Test
testFromListEq = testProperty "== over fromList" $
    \refs -> fromList refs == fromList refs

testFromListNeq :: Test
testFromListNeq = testProperty "/= over fromList" $ forAll neqRefs $
    \(refs1, refs2) -> fromList refs1 /= fromList refs2
  where
    neqRefs = suchThat arbitrary $ \(refs1, refs2) -> refs1 /= refs2

testToListFromList :: Test
testToListFromList = testProperty "toList -> fromList round trip" $
    \ptr -> ptr == fromList (toList ptr)

testToJSONFromJSON :: Test
testToJSONFromJSON = testProperty "toJSON -> fromJSON round trip" $
    \ptr -> case fromJSON $ toJSON (ptr :: Pointer) of
        Success ptr' -> ptr' == ptr
        _ -> False

testIsString :: Test
testIsString = testProperty "toJSON -> fromString round trip" $
    \ptr -> case fromJSON $ toJSON (ptr :: Pointer) of
        Success s -> fromString s == ptr
        _ -> error "Can't parse String from toJSON'd Pointer"

testIndex :: Test
testIndex = testProperty "index finds the right element" $
    \(PVV ptr p t) -> case parse (index p) ptr of
        Success v -> v == t
        _ -> error "Can't index PVV parent with pointer"


data PVV = PVV
    Pointer
    Value -- parent
    Value -- target
    deriving (Show)

-- generated pointers tend to be shallow, TODO fix it
instance Arbitrary PVV where
    arbitrary = do
        p <- arbitrary
        (refs, t) <- target [] p
        return $ PVV (fromList refs) p t
      where
        target refs v@(Object o) = case H.toList o of
            [] -> return (refs, v)
            kvs -> do
                (ref, v') <- elements kvs
                target (refs ++ [ref]) v'
        target refs v@(Array a) = case V.toList a of
            [] -> return (refs, v)
            vs -> do
                (ref, v') <- elements $ deco vs
                target (refs ++ [ref]) v'
        target refs v = return (refs, v)
        deco = zip $ map (T.pack . show) [(0 :: Int) ..]


instance Arbitrary Pointer where
    arbitrary = fmap fromList arbitrary

instance Arbitrary Value where
    arbitrary = sized $ \n -> let n' = n `div` 2 in oneof
        [ fmap (Object . H.fromList) $ resize n' arbitrary
        , fmap (Array . V.fromList) $ resize n' arbitrary
        , fmap String $ arbitrary
        , fmap Number $ arbitrary
        , fmap Bool $ arbitrary
        , return Null ]

instance Arbitrary Number where
    arbitrary = oneof [fmap I arbitrary, fmap D arbitrary]

instance Arbitrary Text where
    arbitrary = fmap T.pack arbitrary
