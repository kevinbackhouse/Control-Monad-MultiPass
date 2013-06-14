-- Copyright 2013 Kevin Backhouse.

module Control.Monad.MultiPass.Example.Assembler
  ( LabelName(..), Register(..), Instruction(..)
  , assemble
  )
where

import Control.Exception ( assert )
import Control.Monad ( liftM )
import Control.Monad.ST2
import Control.Monad.MultiPass
import Control.Monad.MultiPass.Utils
import Control.Monad.MultiPass.Instrument.Delay
import Control.Monad.MultiPass.Instrument.EmitST2ArrayFxp
import Control.Monad.MultiPass.Instrument.Monoid2
import qualified Data.Map as FM
import Data.Maybe ( fromJust )
import Data.Ix
import Data.Word
import Data.Bits
import Data.Monoid

newtype LabelName
  = LabelName String
    deriving (Eq,Ord)

instance Show LabelName where
  show (LabelName name) = name

newtype Register
  = Register Int

instance Show Register where
  show (Register k) = "r" ++ show k

data Instruction
  = Label LabelName
  | Goto LabelName
  | AddImm8 Register Word8
    deriving Show

newtype Addr
  = Addr Word64
    deriving (Eq,Ord,Ix)

instance Num Addr where
  (Addr x) + (Addr y) = Addr (x + y)
  (Addr x) - (Addr y) = Addr (x - y)
  (Addr x) * (Addr y) = Addr (x * y)
  negate (Addr x) = Addr (negate x)
  abs (Addr x) = Addr (abs x)
  signum (Addr x) = Addr (signum x)
  fromInteger x = Addr (fromInteger x)

instance Show Addr where
  show (Addr x) = show x

newtype LabelMap
  = LabelMap (FM.Map LabelName Addr)

lookupLabel :: LabelMap -> LabelName -> Addr
lookupLabel (LabelMap table) key =
  assert (FM.member key table) $
  fromJust (FM.lookup key table)

singletonLabelMap :: LabelName -> Addr -> LabelMap
singletonLabelMap key val =
  LabelMap $ FM.singleton key val

instance Monoid LabelMap where
  mempty =
    LabelMap FM.empty

  mappend (LabelMap xs) (LabelMap ys) =
    assert (FM.null (FM.intersection xs ys)) $
    LabelMap (FM.union xs ys)

type EmitInstrsType r w p1 p2 p3 tc
  =  EmitST2ArrayFxp Addr Word8 r w p1 p2 p3 tc
  -> Monoid2 LabelMap r w p2 p3 tc
  -> Delay p2 p3 tc
  -> MultiPassMain r w tc (p3 (ST2Array r w Addr Word8))

newtype EmitInstrs r w p1 p2 p3 tc =
  EmitInstrs (EmitInstrsType r w p1 p2 p3 tc)

instance MultiPassAlgorithm
           (EmitInstrs r w p1 p2 p3 tc)
           (EmitInstrsType r w p1 p2 p3 tc)
           where
  unwrapMultiPassAlgorithm (EmitInstrs f) = f

assemble
  :: NumThreads
  -> ST2Array r w Int Instruction
  -> ST2 r w (ST2Array r w Addr Word8)
assemble nThreads instructions =
  run $ PassS $ PassS $ PassS $ PassZ $
  EmitInstrs $ \emitter labelMap delay12 ->
  mkMultiPassMain
    (return ())
    (\() ->
     mapST2ArrayMP_ nThreads instructions $
       emitInstr emitter labelMap delay12)
    (\() -> getResult emitter)

emitInstr
  :: (Monad p1, Monad p2, Monad p3)
  => EmitST2ArrayFxp Addr Word8 r w p1 p2 p3 tc
  -> Monoid2 LabelMap r w p2 p3 tc
  -> Delay p2 p3 tc
  -> Instruction
  -> MultiPass r w tc ()
emitInstr emitter labelMap delay12 instruction =
  case instruction of
    AddImm8 r k
      -> emitList emitter (return 4) $
         let r' = emitRegister r in
         return $
           encodeOpcodeWithREX 1 0x83 3 0 r' ++ [k]

    Label label
      -> do addr <- getIndex emitter
            tell labelMap $ liftM (singletonLabelMap label) addr

    Goto label
      -> do pCurrAddr <- getIndex emitter
            pLabels <- listen labelMap
            emitList emitter (return 2) $
              do currAddr <- delay delay12 pCurrAddr
                 labels <- pLabels
                 let gotoAddr = lookupLabel labels label
                 -- The 2-byte JMP instruction can only be used if the
                 -- relative offset can be represented as a signed
                 -- 8-bit number. Note that the offset is calculated
                 -- from the start of the next instruction, so
                 -- currAddr needs to be incremented by 2 for this
                 -- case.
                 let Addr offset = gotoAddr - (currAddr + 2)
                 return $
                   if fitsSignedInt8 offset
                      then [0xEB, fromIntegral offset]
                      else -- Emit a 5-byte instruction. The offset
                           -- needs to be updated accordingly.
                           0xE9 : emitInt32 (offset - 3)

-- Encode the first three bytes of an instruction with a REX prefix:
--
--   1. REX prefix
--   2. Instruction opcode
--   3. ModR/M byte
--
encodeOpcodeWithREX
  :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> [Word8]
encodeOpcodeWithREX w opcode md reg rm =
  assert (w < 2) $
  assert (md < 4) $
  assert (reg < 16) $
  assert (rm < 16) $
  [ -- REX prefix
    0x40 .|. shiftL w 3 .|.
    shiftR (reg .&. 8) 1 .|. shiftR (rm .&. 8) 3

  , opcode

    -- ModR/M byte
  , shiftL md 6 .|. shiftL (reg .&. 7) 3 .|. (rm .&. 7)
  ]

emitInt32 :: (Integral w, Bits w) => w -> [Word8]
emitInt32 = emitWord 4

-- Emit lowest n bytes of x, with the least significant byte at the
-- head of the list.
emitWord
  :: (Integral w, Bits w)
  => Int
  -> w
  -> [Word8]
emitWord n x =
  if n == 0
     then []
     else fromIntegral x : emitWord (n-1) (x `shiftR` 8)

-- Convert the register to its 4-bit encoding.
emitRegister :: Register -> Word8
emitRegister (Register r) = fromIntegral r

-- Return true if the number is representable as an Int8. 
fitsSignedInt8 :: Integral w => w -> Bool
fitsSignedInt8 k =
  k == signExtend8 (fromIntegral k)

-- Sign extend a Word8.
signExtend8 :: Num w => Word8 -> w
signExtend8 x =
  if x .&. 0x80 == 0
     then fromIntegral x
     else -(fromIntegral (-x))
