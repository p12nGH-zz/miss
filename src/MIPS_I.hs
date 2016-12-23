module MIPS_I where

import Control.Monad.ST
import Data.Array.IO
import Data.Array.MArray
import Data.Word
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Data.Bits
import Data.Int
import Data.IORef

type Memory = IOUArray Word32 Word8

data CPU_State = CPU_State {
    gpr :: (IOUArray Word32 Word32),
    mem :: Memory 
}
type BF = Word32 --bit field type
data Instruction
    = R {opcR :: BF, rsR :: BF, rtR :: BF, rdR :: BF, shamtR :: BF, functR :: BF}
    | I {opcI :: BF, rsI :: BF, rtI :: BF, immiI :: BF}
    | J {opcJ :: BF, addrJ :: BF} deriving (Show)

type R a = ReaderT CPU_State IO a

-- read register
rr :: Word32 -> R Word32
rr 0 = return 0
rr n = do
    regs <- gpr <$> ask
    lift $ readArray regs n

wr :: Word32 -> Word32 -> R ()
wr n v = do
    regs <- gpr <$> ask
    lift $ writeArray regs n v

evalI :: Instruction -> R ()
evalI (R _ rs rt rd shamt funct) = (opsR funct shamt) <$> rr rs <*> rr rt >>= wr rd

opsR 0 shamt = f where
    f _ t = shiftL t shamt
opsR 2 shamt = f where
    f _ t = shiftR t shamt
opsR funct _ = opsRS funct

opsRS 4  = shiftL
opsRS 6  = shiftR
opsRS 20 = signedOp (+)
opsRS 21 = (+)
opsRS 22 = signedOp (-)
opsRS 23 = (-)
opsRS 24 = (.&.)
opsRS 25 = (.|.)

shiftL x s = shift x (fromIntegral s)
shiftR x s = shift x (fromIntegral $ s * (-1))

signedOp op = f where
    toSigned = fromIntegral :: (Word32 -> Int32)
    toUnsigned = fromIntegral :: (Int32 -> Word32)
    f a b = toUnsigned $ op (toSigned a) (toSigned b)

readDecode :: Word32 -> Memory -> IO Instruction
readDecode a mem = do
    let
        rm :: Word32 -> IO Word32
        rm o = fromIntegral <$> readArray mem (a + o)
    (b0, b1, b2, b3) <- (,,,) <$> rm 0 <*> rm 1 <*> rm 2 <*> rm 3
    
    let
        dw = b0 .|. (shift b1 8) .|. (shift b2 16) .|. (shift b3 24) 
        opc = (shift b3 (-2)) .&. 0x3f
        
        funct = b0 .&. 0x3f
        shamt = (shift dw 6) .&. 0x1f
        rd = (shift dw 11) .&. 0x1f
        rt = b2 .&. 0x1f
        rs = (shift dw 21) .&. 0x1f
        imm = dw .&. 0xffff
        addr = dw .&. 0x3ffffff
        --rs = (shift b3 3) .&.
    
        instr = case opc of
            0 -> R 0 rs rt rd shamt funct
            2 -> J 2 addr 
            3 -> J 3 addr
            _ -> I opc rs rt imm
    return $ instr

