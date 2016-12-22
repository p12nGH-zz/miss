{-# LANGUAGE Strict #-}

import Control.Monad.ST
import Data.Array.IO
import Data.Array.MArray
import Data.Word
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)

data CPU_State = CPU_State {
    gpr :: (IOUArray Word32 Word32),
    mem :: (IOUArray Word32 Word8)
}

type R a = ReaderT CPU_State IO a

r1 :: R Word32
r1 = do
    m <- gpr <$> ask
    lift $ readArray m 0
    

t1 = do
    regs <- newArray (0, 10) 12 :: IO (IOUArray Word32 Word32)
    mem <- newArray (0,10) 12 :: IO (IOUArray Word32 Word8)
    runReaderT r1 $ CPU_State regs mem

main = do
    t1 >>= print
