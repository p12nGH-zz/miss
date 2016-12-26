module Elf where

import qualified Data.Binary.Get as BG
import qualified Data.ByteString.Lazy as B (readFile, unpack)
import Data.Array.IO
import Data.Array.MArray
import Data.Word
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Exception

gw = fmap fromIntegral BG.getWord32be

-- returns (p_type, p_offset, p_filesz)
readPH = (,,) <$> gw <*> gw <*> (BG.skip 0x8 *> gw <* BG.skip 0xc)

exactByte b = do
    rb <- BG.getWord8
    if rb == b then return () else fail "ELF parsing error"

parseElfHeader = do
    forM [0x7F, 0x45, 0x4c, 0x46] exactByte -- check ELF file signature
    BG.skip 0x18 -- go to e_phoff field
    (,) <$> gw <*> (BG.skip 0xc *> gw)
    
readHeadersTable (e_phoff, e_phnum) = do
    BG.skip e_phoff -- go to headers table
    headers <- replicateM (fromIntegral e_phnum) readPH
    let
        load_headers = filter (\(t, _, _) -> t == 0x1) headers -- PT_LOAD
    return load_headers

readSegment (p_type, p_offset, p_filesz) = do
    BG.skip p_offset
    BG.getLazyByteString $ fromIntegral p_filesz

-- read first loadable segment from an ELF file
-- and store its content in IOUArray

readElfFile :: String -> ExceptT String IO (IOUArray Word32 Word8)
readElfFile filename = do
    content <- lift $ B.readFile filename
    let
        parse p = (\(_, _, a) -> a) <$> (BG.runGetOrFail p content)
        parseElf = do
            h@(e_phoff, e_phnum) <- parse parseElfHeader
            headers <- parse (readHeadersTable h)
            parse $ readSegment $ head headers
    image <- case parseElf of
        Right r -> return r
        Left _ -> throwE "ELF parsing error"
   
        
    lift $ print $ BG.runGet parseElfHeader content
    lift $ newListArray (0, 0x10000) $ B.unpack image


