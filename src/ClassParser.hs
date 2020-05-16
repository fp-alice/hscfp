module ClassParser where
import           Internal.ConstantPool
import           Control.Monad        (guard, replicateM)
import           Data.Binary          (Word16, Word32, Word8)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy as BL
import           Internal.Attributes  (AttributeInfo (..), getAttributeInfo)

-- | Holds field access flags, name index, descriptor index, and attributes
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.5
data FieldInfo = FieldInfo Word16 Word16 Word16 Word16 [AttributeInfo] deriving (Show, Eq)

x = do
  y <- getW

-- | Parser for fields
getField :: ConstantPool -> Get FieldInfo
getField pool = do
  fieldFlags                <- getWord16be
  fieldNameIndex            <- getWord16be
  fieldDescriptorIndex      <- getWord16be
  attributesCount           <- getWord16be
  attributesInfo            <- replicateN attributesCount (getAttributeInfo pool)
  return $ FieldInfo fieldFlags fieldNameIndex fieldDescriptorIndex attributesCount attributesInfo

-- | Holds method flags, name index, descriptor index, and attributes
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.6
data MethodInfo = MethodInfo Word16 Word16 Word16 Word16 [AttributeInfo] deriving (Show, Eq)

-- | Parser for methods
getMethod :: ConstantPool -> Get MethodInfo
getMethod pool = do
  flags           <- getWord16be
  nameIndex       <- getWord16be
  descriptorIndex <- getWord16be
  attributesCount <- getWord16be
  attributesInfo  <- replicateN attributesCount (getAttributeInfo pool)
  return $ MethodInfo flags nameIndex descriptorIndex attributesCount attributesInfo

-- | Parses version information, jvm does (minor, major), but I want (major, minor)
getVersion :: Get (Int, Int)
getVersion = do
  minor <- fromIntegral <$> getWord16be
  major <- fromIntegral <$> getWord16be
  return (major, minor)

-- | The unmodified, parsed binary class file
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.1
data ParsedClass = ParsedClass {
  classVersion           :: (Int, Int),
  classConstantPoolCount :: Word16,
  classConstantPoolInfo  :: ConstantPool,
  classAccessFlags       :: Word16,
  classThisClass         :: Word16,
  classSuperClass        :: Word16,
  classInterfacesCount   :: Word16,
  classInterfaces        :: [Word16],
  classFieldsCount       :: Word16,
  classFieldsInfo        :: [FieldInfo],
  classMethodsCount      :: Word16,
  classMethodsInfo       :: [MethodInfo],
  classAttributesCount   :: Word16,
  classAttributesInfo    :: [AttributeInfo]
} deriving (Show, Eq)

-- | Parses the body of a class file
getClass = do
  version        <- getVersion
  numConstants   <- getWord16be
  constants      <- getConstantPool (numConstants - 1)
  classAccess    <- getWord16be
  thisClass      <- getWord16be
  superClass     <- getWord16be
  numInterfaces  <- getWord16be
  interfaces     <- replicateN numInterfaces getWord16be
  numFields      <- getWord16be
  fields         <- replicateN numFields (getField constants)
  numMethods     <- getWord16be
  methods        <- replicateN numMethods (getMethod constants)
  attributeCount <- getWord16be
  attributes     <- replicateN attributeCount (getAttributeInfo constants)
  return $ ParsedClass version numConstants constants classAccess thisClass superClass numInterfaces interfaces numFields fields numMethods methods attributeCount attributes

-- | Parses an entire class file checking that it has the magic number, failing otherwise
tryGetClass = do
  magic <- getWord32be
  if magic /= 0xCAFEBABE
    then fail "file is missing magic number"
    else getClass

runClass = runGet tryGetClass <$> classData
  where
    classData = BL.readFile "Main.class"
