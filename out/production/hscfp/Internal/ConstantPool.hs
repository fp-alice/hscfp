module Internal.ConstantPool
  ( ConstantPool
  , Constant(..)
  , poolConstantUtf8
  , poolConstantClass
  , replicateN
  , getConstantPool
  ) where

import           Control.Monad   (replicateM)
import           Data.Binary     (Word16, Word8, get)
import           Data.Binary.Get
import           Data.Int        (Int32, Int64)
import qualified Data.Map        as Map
import           Data.ByteString.UTF8 (toString)

-- | Type for every possible entry in the constant pool
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.4-210
data Constant
  = ConstantUtf8 String
  | ConstantInteger Int32
  | ConstantFloat Float
  | ConstantLong Int64
  | ConstantDouble Double
  | ConstantClass Word16
  | ConstantString Word16
  | ConstantFieldRef Word16 Word16
  | ConstantMethodRef Word16 Word16
  | ConstantInterfaceMethodRef Word16 Word16
  | ConstantNameAndType Word16 Word16
  | ConstantMethodHandle Word8 Word16
  | ConstantMethodType Word16
  | ConstantInvokeDynamic Word16 Word16
  deriving (Show, Eq)

-- | Indexed constant pool type
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.4
type ConstantPool = Map.Map Word16 Constant

-- | replicateM but it takes a Word instead of an int
replicateN :: (Integral i, Applicative m) => i -> m b -> m [b]
replicateN n = replicateM (fromIntegral n)

-- | Parser for every possible entry in the constant pool
getConstant :: Get Constant
getConstant = do
  tag <- getWord8
  case tag of
    1  -> ConstantUtf8 . toString <$> (getByteString . fromIntegral =<< getWord16be)
    3  -> ConstantInteger <$> get
    4  -> ConstantFloat <$> get
    5  -> ConstantLong <$> get
    6  -> ConstantDouble <$> get
    7  -> ConstantClass <$> getWord16be
    8  -> ConstantString <$> getWord16be
    9  -> ConstantFieldRef <$> getWord16be <*> getWord16be
    10 -> ConstantMethodRef <$> getWord16be <*> getWord16be
    11 -> ConstantInterfaceMethodRef <$> getWord16be <*> getWord16be
    12 -> ConstantNameAndType <$> getWord16be <*> getWord16be
    15 -> ConstantMethodHandle <$> getWord8 <*> getWord16be
    16 -> ConstantMethodType <$> getWord16be
    18 -> ConstantInvokeDynamic <$> getWord16be <*> getWord16be
    _  -> bytesRead >>= \pos -> fail ("Encountered invalid type tag \'" ++ show tag ++ "\' at position " ++ show pos)

-- | Parses an entire constant pool of size n into a Word16-indexed Map starting at index 1 instead of 0
getConstantPool :: Word16 -> Get ConstantPool
getConstantPool n = Map.fromList . zip ([1 ..] :: [Word16]) <$> replicateN n getConstant

-- | Functions for accessing the constant pool

poolConstantUtf8 :: ConstantPool -> Word16 -> String
poolConstantUtf8 pool index =
  case pool Map.! index of
    ConstantUtf8 s -> s
    _              -> error "Index is not a ConstantUtf8"

poolConstantClass :: ConstantPool -> Word16 -> String
poolConstantClass pool index =
  case pool Map.! index of
    ConstantClass pos -> poolConstantUtf8 pool pos
    _                 -> error "Index is not a ConstantClass"
