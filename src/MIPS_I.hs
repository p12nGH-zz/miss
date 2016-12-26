{-# LANGUAGE Strict #-}

module MIPS_I (
    run
)
where

import Control.Monad.ST
import Data.Array.IO
import Data.Array.MArray
import Data.Word
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Control.Monad
import Data.Bits
import Data.Int
import Data.IORef

type Memory = IOUArray Word32 Word8

data CPU_State = CPU_State {
    gpr :: (IOUArray Word32 Word32),
    mem :: Memory,
    hi, lo, ip :: IORef Word32 
}
type BF = Word32 --bit field type
data Instruction
    = R {opcR :: BF, rsR :: BF, rtR :: BF, rdR :: BF, shamtR :: BF, functR :: BF}
    | I {opcI :: BF, rsI :: BF, rtI :: BF, immiI :: BF}
    | J {opcJ :: BF, addrJ :: BF} deriving (Show)

-- processor state monad
type R a =  ReaderT CPU_State (ExceptT String IO) a

state :: R CPU_State
state = ask

io :: IO a -> R a
io = lift . lift

--helper functions to access special registers
rSpecial r = (read, write) where
    read = r <$> state >>= io . readIORef
    write v = r <$> state >>= (\h -> io $ writeIORef h v)
(rIP, wIP) = rSpecial ip
(rHI, wHI) = rSpecial hi
(rLO, wLO) = rSpecial lo

-- read register
rr :: Word32 -> R Word32
rr 0 = return 0
rr n = do
    regs <- gpr <$> state
    io $ readArray regs n

--write register
wr :: Word32 -> Word32 -> R ()
wr n v = do
    regs <- gpr <$> state
    io $ writeArray regs n v

evalI :: Instruction -> R ()

-- memory load instructions
evalI (I 0x23 rs rt imm) = ((+) imm) <$> rr rs >>= readMem32 >>= wr rt
evalI (I 0x21 rs rt imm) = ((+) imm) <$> rr rs >>= \a -> se16 <$> readMem16 a >>= wr rt
evalI (I 0x25 rs rt imm) = ((+) imm) <$> rr rs >>= \a -> fromIntegral <$> readMem16 a >>= wr rt
evalI (I 0x20 rs rt imm) = ((+) imm) <$> rr rs >>= \a -> se8 <$> readMem8 a >>= wr rt
evalI (I 0x24 rs rt imm) = ((+) imm) <$> rr rs >>= \a -> fromIntegral <$> readMem8 a >>= wr rt

-- memory store instructions
evalI (I 0x28 rs rt imm) = do
    addr <- ((+) imm) <$> rr rs
    d <- rr rt
    writeMem8 addr (fromIntegral d)
evalI (I 0x29 rs rt imm) = do
    addr <- ((+) imm) <$> rr rs
    d <- rr rt
    writeMem16 addr (fromIntegral d)
evalI (I 0x2b rs rt imm) = do
    addr <- ((+) imm) <$> rr rs
    d <- rr rt
    writeMem32 addr d

-- jump instructions
evalI (I 0x4 rs rt imm) = do
    c <- (==) <$> rr rs <*> rr rt
    if c then upIP (imm * 4) else return ()
evalI (I 0x5 rs rt imm) = do
    c <- (/=) <$> rr rs <*> rr rt
    if c then upIP (imm * 4) else return ()

evalI (J 0x2 addr) = upIP (addr * 4 - 4)
evalI (J 0x3 addr) = do
    n <- rIP
    wr 31 (n + 4)
    upIP (addr * 4 - 4)
evalI (R 0x8 rs _ _ _ _) = do
    n <- rr rs
    wIP (n - 4)

-- logit and arithmetic
evalI (I 0x8 rs rt imm) = (signedOp (+) (seImm imm)) <$> rr rs >>= wr rt
evalI (I 0x9 rs rt imm) = (signedOp (+) (seImm imm)) <$> rr rs >>= wr rt
evalI (I 0xc rs rt imm) = ((.&.) imm) <$> rr rs >>= wr rt
evalI (I 0xd rs rt imm) = ((.|.) imm) <$> rr rs >>= wr rt
evalI (I 0xe rs rt imm) = (xor imm) <$> rr rs >>= wr rt
evalI (I 0xa rs rt imm) = do
  s <- rr rs
  wr rt (if s < imm then 1 else 0)
evalI (R _ _ _ rd _ 0x10) = rHI >>= wr rd
evalI (R _ _ _ rd _ 0x12) = rLO >>= wr rd
evalI (R _ rs rt _ _ 0x1a) = do
    quot <$> rr rs <*> rr rt >>= wLO
    rem  <$> rr rs <*> rr rt >>= wHI
evalI (R _ rs rt _ _ 0x1b) = do
    (signedOp quot) <$> rr rs <*> rr rt >>= wLO
    (signedOp rem)  <$> rr rs <*> rr rt >>= wHI
evalI (R _ rs rt rd shamt funct) = (opsR funct shamt) <$> rr rs <*> rr rt >>= wr rd

opsR 0x0 shamt = \_ t -> shiftL' t shamt
opsR 0x2 shamt = \_ t -> shiftR' t shamt
opsR 0x3 shamt = \_ t -> (signedOp shiftR') t shamt
opsR 0x7 shamt = \_ t -> (signedOp shiftL') t shamt
opsR funct _ = opsRS funct

opsRS 0x4  = shiftL'
opsRS 0x6  = shiftR'
opsRS 0x20 = signedOp (+)
opsRS 0x21 = (+)
opsRS 0x22 = signedOp (-)
opsRS 0x23 = (-)
opsRS 0x24 = (.&.)
opsRS 0x25 = (.|.)
opsRS 0x26 = xor
opsRS 0x27 = \s t -> complement $ s .|. t
opsRS 0x2a = \s t -> if s < t then 1 else 0
opsRS 0x2b = signedOp $ \s t -> if s < t then 1 else 0

upIP n = do
  o <- rIP
  wIP $ o + n

-- ALU helper functions

shiftL' x s = shift x (fromIntegral s)
shiftR' x s = shift x (fromIntegral $ s * (-1))

-- sign extend to 32 bits
se8 = (fromIntegral :: Int32 -> Word32)
    . (fromIntegral :: Int8 -> Int32)
    . (fromIntegral :: Word8 -> Int8)
se16 = (fromIntegral :: Int32 -> Word32)
     . (fromIntegral :: Int16 -> Int32)
     . (fromIntegral :: Word16 -> Int16)
seImm w = se16 $ fromIntegral $ w .|. 0xffff

-- Memory
readMem8 :: Word32 -> R Word8
readMem8 addr = do
    m <- mem <$> state
    io $ readArray m (fromIntegral addr)

readMem16 :: Word32 -> R Word16
readMem16 addr = do
    l <- fromIntegral <$> (readMem8 addr)
    h <- fromIntegral <$> (readMem8 $ addr + 1)
    return $ (shift 8 h) .|. l

readMem32 :: Word32 -> R Word32
readMem32 addr = do
    l <- fromIntegral <$> (readMem16 addr)
    h <- fromIntegral <$> (readMem16 $ addr + 2)
    return $ (shiftL 16 h) .|. l

writeMem8 :: Word32 -> Word8 -> R ()
writeMem8 addr d = do
    m <- mem <$> state
    io $ writeArray m addr d

writeMem16 :: Word32 -> Word32 -> R ()
writeMem16 addr d = do
    writeMem8 addr $ fromIntegral d
    writeMem8 (addr + 1) $ fromIntegral $ shiftR d 8

writeMem32 :: Word32 -> Word32 -> R ()
writeMem32 addr d = do
    writeMem16 addr $ fromIntegral d
    writeMem16 (addr + 2) $ fromIntegral $ shiftR d 16

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

fetchEval = do
    m <- mem <$> state
    ip <- rIP
    io $ print ip
    instr <- io $ readDecode ip m
    evalI instr
    upIP 4

run :: Memory -> ExceptT String IO ()
run m = do
    gpr <- lift $ newArray (0, 31) 0
    hi <- lift $ newIORef 0
    lo <- lift $ newIORef 0
    ip <- lift $ newIORef 0

    runReaderT (replicateM_ 20 fetchEval) (CPU_State gpr m hi lo ip)

