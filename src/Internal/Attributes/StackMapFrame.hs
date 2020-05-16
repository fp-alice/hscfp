module Internal.Attributes.StackMapFrame (VerificationTypeInfo(..), StackMapFrame(..), getStackMapFrame) where

import           Internal.ConstantPool    (ConstantPool, replicateN)
import           Data.Binary     (Word16, Word8)
import           Data.Binary.Get (Get, getWord16be, getWord8)

-- | ADT capable of representing all VerificationTypeInfo union types
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.7.4
data VerificationTypeInfo = TopVariableInfo Word8
                          | IntegerVariableInfo Word8
                          | FloatVariableInfo Word8
                          | LongVariableInfo Word8
                          | DoubleVariableInfo Word8
                          | NullVariableInfo Word8
                          | UninitializedThisVariableInfo Word8
                          | ObjectVariableInfo Word8 Word16
                          | UninitializedVariableInfo Word8 Word16
                          deriving (Show, Eq)

-- | ADT capable of representing all StackMapFrame union types
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.7.4
data StackMapFrame = SameFrame Word8
                   | SameLocals1StackItemFrame Word8 [VerificationTypeInfo]
                   | SameLocals1StackItemFrameExtended Word8 [VerificationTypeInfo]
                   | ChopFrame Word8 Word16
                   | SameFrameExtended Word8 Word16
                   | AppendFrame Word8 Word16 [VerificationTypeInfo]
                   | FullFrame Word8 Word16 Word16 [VerificationTypeInfo] Word16 [VerificationTypeInfo]
                   deriving (Show, Eq)

-- | Parses verification type info discriminating on first byte
getVerificationTypeInfo :: Get VerificationTypeInfo
getVerificationTypeInfo = do
  tag <- getWord8
  case tag of
    0 -> return (TopVariableInfo tag)
    1 -> return (IntegerVariableInfo tag)
    2 -> return (FloatVariableInfo tag)
    3 -> return (DoubleVariableInfo tag)
    4 -> return (LongVariableInfo tag)
    5 -> return (NullVariableInfo tag)
    6 -> return (UninitializedThisVariableInfo tag)
    7 -> ObjectVariableInfo tag <$> getWord16be
    8 -> UninitializedVariableInfo tag <$> getWord16be
    _ -> error ("Unexpected verification type info tag " ++ show tag)

getSameFrame :: Word8 -> Get StackMapFrame
getSameFrame frameType = return (SameFrame frameType)

getSameLocals1StackItemFrame :: Word8 -> Get StackMapFrame
getSameLocals1StackItemFrame frameType =
  SameLocals1StackItemFrame frameType <$> replicateN 1 getVerificationTypeInfo

getSameLocals1StackItemFrameExtended :: Word8 -> Get StackMapFrame
getSameLocals1StackItemFrameExtended frameType =
  SameLocals1StackItemFrameExtended frameType <$> replicateN 1 getVerificationTypeInfo

getChopFrame :: Word8 -> Get StackMapFrame
getChopFrame frameType = ChopFrame frameType <$> getWord16be

getSameFrameExtended :: Word8 -> Get StackMapFrame
getSameFrameExtended frameType = SameFrameExtended frameType <$> getWord16be

getAppendFrame :: Word8 -> Get StackMapFrame
getAppendFrame frameType =
  AppendFrame frameType <$> getWord16be <*> replicateN (frameType - 251) getVerificationTypeInfo

getFullFrame :: Word8 -> Get StackMapFrame
getFullFrame frameType = do
  offsetDelta        <- getWord16be
  numberOfLocals     <- getWord16be
  localTypeInfo      <- replicateN numberOfLocals getVerificationTypeInfo
  numberOfStackItems <- getWord16be
  stackItemsTypeInfo <- replicateN numberOfStackItems getVerificationTypeInfo
  return (FullFrame frameType offsetDelta numberOfLocals localTypeInfo numberOfStackItems stackItemsTypeInfo)

-- | Discriminates frames based on the value of the first byte
frameDiscriminator :: Word8 -> Get StackMapFrame
frameDiscriminator frameType
  | frameType <= 63  = getSameFrame frameType
  | frameType <= 127 = getSameLocals1StackItemFrame frameType
  | frameType == 247 = getSameLocals1StackItemFrameExtended frameType
  | frameType <= 250 = getChopFrame frameType
  | frameType == 251 = getSameFrameExtended frameType
  | frameType <= 254 = getAppendFrame frameType
  | frameType == 255 = getFullFrame frameType

-- | Parser that reads in any possible stack map frames
getStackMapFrame :: Get StackMapFrame
getStackMapFrame = getWord8 >>= frameDiscriminator
