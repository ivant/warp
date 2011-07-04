{-# LANGUAGE ViewPatterns, OverloadedStrings, RankNTypes, DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>))
import Data.Functor.Identity
import Data.List ((\\))
import Data.String (fromString)
import Debug.Trace
import Network.HTTP.Types (Status, status500, statusOK)
import Network.Wai (Application, Response, responseLBS)
import Network.Wai.Handler.Warp
import qualified Data.ByteString as SB; import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Enumerator as E; import Data.Enumerator (Enumeratee, Iteratee, Stream(..), Step(..), run_, ($=), (=$), ($$))
import qualified Data.Enumerator.List as EL
import Test.QuickCheck

import Network.Wai.Handler.ChunkedEncoding

main :: IO ()
main = do
    quickCheck prop_httpChunkedEncodingWithDecodingIsIdentity
    putStrLn "Starting server on port 1234 which returns the request body length in the response (or an error when the body is empty)..."
    run 1234 testR
  where
    testR :: Application
    testR req = do
        bs <- EL.consume
        let len = (BC8.length $ SB.concat bs)
        if len == 0 then
            respondWithText status500 "Empty request body"
          else
            respondWithText statusOK $ "Length: " ++ show len

-- | Test details:
-- 1. Generate random text
-- 2. Split it into random parts
-- 3. Produce HTTP chunked encoding of these parts
-- 4. Split it into random parts
-- 5. Feed these parts as Enumerator chunks to the fromHttpChunked
-- 6. Get the final result, and compare with initial text
prop_httpChunkedEncodingWithDecodingIsIdentity :: Property
prop_httpChunkedEncodingWithDecodingIsIdentity =
    forAll genText $ \text ->
    forAll (chunksGen text) $ \chunks ->
    forAll (choose (1, length chunks)) $ \(fromIntegral -> chunksPerCall) ->
    forAll (choose (8, 16384)) $ \mss ->
    let iter = ((E.enumList chunksPerCall chunks $= toHttpChunkedWithMSS mss) $= fromHttpChunked) $$ concatIteratee
        iterResult = runIdentity (E.run iter)
      in
        case iterResult of
          Right recoveredText -> printTestCase ("Incorrect chunked parse: " ++ show (text, recoveredText, chunks)) $ text == recoveredText
          Left e              -> printTestCase ("Error while parsing chunked: " ++ show (e, text, chunks)) $ False
  where
    concatIteratee :: Monad m => Iteratee ByteString m ByteString
    concatIteratee = SB.concat <$> EL.consume

-- | Generates a list of non-negative integers which split the given integer; may contain zeroes.
genSplits :: Int -> Gen [Int]
genSplits 0 = return []
genSplits n = do
    m <- choose (0,n)
    (m:) <$> genSplits (n-m)

-- | Splits a ByteString into parts of random length
chunksGen :: ByteString -> Gen [ByteString]
chunksGen bs  | SB.null bs = return []
              | otherwise  = do
                  ss <- genSplits (SB.length bs)
                  let ps = scanl (+) 0 ss
                      posLens = zip ps ss
                  return $ map (\(p,l) -> SB.take l (SB.drop p bs)) posLens

genText :: Gen ByteString
genText = (BC8.pack . concat) <$> listOf (frequency [ (19, elements (map (:"") vowels))
                                                    , (31, elements (map (:"") consonants))
                                                    , (7,  return " ")
                                                    , (2,  elements ["\n", "\r", "\r\n"])
                                                    ])
  where
    vowels = "aeiou"
    consonants = ['a'..'z'] \\ vowels

respondWithText :: (Monad m) => Status -> String -> m Response
respondWithText status str = return $ responseLBS status stdHeaders $ fromString str
  where
    stdHeaders = [("Content-Type", "text/plain")]

