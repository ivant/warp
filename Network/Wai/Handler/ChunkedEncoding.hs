{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, DeriveDataTypeable #-}
module Network.Wai.Handler.ChunkedEncoding (
  fromHttpChunked,
  toHttpChunked,
  toHttpChunkedWithMSS,
  ChunkedParseFailed(..)
) where

import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Control.Monad (guard, unless)
import Control.Monad.Trans.Class (lift)
import Data.Char (chr, ord, isHexDigit)
import Data.List (unfoldr)
import Data.String (fromString)
import Data.Tuple (swap)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Numeric (readHex)
import qualified Data.ByteString as SB; import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.Enumerator as E; import Data.Enumerator (Enumeratee, Iteratee, Stream(..), Step(..))
import qualified Data.Enumerator.Binary as EB
--import qualified Data.Enumerator.List as EL

newtype ChunkedParseFailed = ChunkedParseFailed String deriving (Show, Typeable)
instance Exception ChunkedParseFailed

-- | The 'fromHttpChunked' is an 'Enumeratee' that converts an HTTP chunked-encoded stream into a raw byte stream.
--
-- Chunk extensions are not supported and are skipped.
fromHttpChunked :: forall b m. Monad m => Enumeratee ByteString ByteString m b
fromHttpChunked = loop
  where
    loop, yieldOrError :: Monad m => Step ByteString m b -> Iteratee ByteString m (Step ByteString m b)
    loop (Continue kont)      = do
        sz <- readSize
        skipChunkExtension
        readCRLF
        chunks <- Chunks . LB.toChunks <$> EB.take sz
        readCRLF

        case sz of
          0 -> lift (E.runIteratee $ kont EOF)    >>= yieldOrError
          _ -> lift (E.runIteratee $ kont chunks) >>= loop

    loop result               = yieldOrError result

    yieldOrError (Continue _) = chunkedError "divergent iteratee"
    yieldOrError result       = return result

    chunkedError :: Monad m => String -> Iteratee ByteString m a
    chunkedError = E.throwError . ChunkedParseFailed

    readCRLF :: Monad m => Iteratee ByteString m ()
    readCRLF = do
        crlf <- EB.take 2
        unless (crlf == "\r\n") $ chunkedError "CRLF expected, but got something else"

    readSize :: Monad m => Iteratee ByteString m Integer
    readSize = do
        hexSize <- fromHex <$> EB.takeWhile (isHexDigit . chr . fromIntegral)
        case hexSize of
          Nothing    -> chunkedError "unexpected character while parsing the size of a chunk"
          Just hexSz -> return hexSz
      where
        fromHex :: LB.ByteString -> Maybe Integer
        fromHex hexStr = case readHex $ LBC8.unpack hexStr of
            [(n, "")] -> Just n
            _         -> Nothing

    skipChunkExtension :: Monad m => Iteratee ByteString m ()
    skipChunkExtension = EB.takeWhile (/= cr) >> return ()

    cr :: Word8
    cr = fromIntegral (ord '\r')

-- | The 'toHttpChunked' is an 'Enumeratee' that converts a byte stream into a chunked-encoded stream.
toHttpChunked :: forall b m. Monad m => Enumeratee ByteString ByteString m b
toHttpChunked = toHttpChunkedWithMSS 1460

-- | The 'toHttpChunkedWithMSS' is an 'Enumeratee' that converts a byte stream into a chunked-encoded stream, using the supplied TCP MSS as a guideline for sizing the chunks.
toHttpChunkedWithMSS :: forall b m. Monad m => Int -> Enumeratee ByteString ByteString m b
toHttpChunkedWithMSS mss = E.checkDone (E.continue . step)
  where
    -- checkDone       :: ((Stream ai -> Iteratee ai m b) -> Iteratee ao m (Step ai m b)) -> Enumeratee ao ai m b
    -- continue . step ::  (Stream ai -> Iteratee ai m b) -> Iteratee ao m (Step ai m b)
    -- continue        ::  (Stream ao -> Iteratee ao m x) -> Iteratee ao m x
    -- step :: (Stream ai -> Iteratee ai m b) -> Stream ao -> Iteratee ao m (Step ai m b)
    step :: Monad m => (Stream ByteString -> Iteratee ByteString m b) -> Stream ByteString -> Iteratee ByteString m (Step ByteString m b)
    step k (Chunks cs) = do
        let outChunks = concat [ [toHex sz, endl] ++ bss ++ [endl]
                               | bss <- splitIntoChunks cs
                               , let sz = sum $ map SB.length bss
                               , sz > 0
                               ]
        next <- lift $ E.runIteratee $ k (Chunks outChunks)
        tryToContinue next $ E.continue . step

    step k EOF = do
        next <- lift $ E.runIteratee $ k (Chunks [endChunked])
        tryToContinue next $ \k' -> lift $ E.runIteratee $ k' EOF

    -- | If the first argument is Continue ki, then pass ki to the second argument, otherwise just return the first argument
    tryToContinue :: Monad m => Step ai m b -> ((Stream ai -> Iteratee ai m b) -> Iteratee ao m (Step ai m b)) -> Iteratee ao m (Step ai m b)
    tryToContinue (Continue ki) ko = ko ki
    tryToContinue r _ = return r

    endl = fromString "\r\n"
    endChunked = SB.concat [fromString "0", endl, endl]

    toHex :: Int -> ByteString
    toHex 0 = fromString "0"
    toHex n = SB.pack $ map toHexChar $ reverse $ unfoldr (\i -> (guard (i>0) >> return (swap (i `quotRem` 16)))) n
      where
        toHexChar c | c < 10    = fromIntegral $ 48 + c
                    | otherwise = fromIntegral $ 65 + c - 10

    -- | Splits the list of ByteStrings into sub-lists, sums of sizes of Bytestrings in which are under preferredMaxChunkSize
    splitIntoChunks :: [ByteString] -> [[ByteString]]
    splitIntoChunks = splitIntoChunks' [] [] 0
      where
        splitIntoChunks' :: [[ByteString]]  -- ^ accumulated sub-lists of chunks (reversed)
                         -> [ByteString]    -- ^ current sub-list which we are accumulating (reversed)
                         -> Int             -- ^ sum of sizes of strings in the current sub-list being accumulating
                         -> [ByteString]    -- ^ input strings
                         -> [[ByteString]]
        splitIntoChunks' cs [] 0 [] = reverse cs
        splitIntoChunks' cs ps _ [] = splitIntoChunks' (reverse ps : cs) [] 0 []
        splitIntoChunks' cs ps s is@(b:bs) | s == preferredMaxChunkSize  = splitIntoChunks' (reverse ps : cs) [] 0 is
                                           | s' <= preferredMaxChunkSize = splitIntoChunks' cs (b:ps) s' bs
                                           | s' > preferredMaxChunkSize  = let (b1,b2) = SB.splitAt (preferredMaxChunkSize-s) b in
                                                                           splitIntoChunks' cs (b1:ps) preferredMaxChunkSize (b2:bs)
                                           | otherwise = error "impossible happened"
          where
            s' = s + SB.length b

    preferredMaxChunkSize = mss - 6 - 2 -- 6 is for chunk size hex number length (4 bytes) plus CRLF after it,
                                        -- 2 is for CRLF after the chunk
                                        -- This assumes that mss is < 16^4 == 65536. That's reasonable,
                                        -- since even Jumbo packets are usually not bigger than 16384.

