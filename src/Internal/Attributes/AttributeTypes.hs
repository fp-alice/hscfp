module Internal.Attributes.AttributeTypes where

import           Control.Applicative               (liftA2, liftA3)
import           Control.Monad                     (guard, replicateM)
import           Data.Binary                       (Word16, Word32, Word8)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy              as BL
import           Data.Char                         (chr)
import           Internal.Attributes.StackMapFrame
import           Internal.ConstantPool

-- | Exception handler info
data ExceptionHandler = ExceptionHandler Word16 Word16 Word16 Word16 deriving (Show, Eq)

getExceptionHandler :: Get ExceptionHandler
getExceptionHandler = ExceptionHandler <$> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be

-- | Inner class info
data InnerClassInfo = InnerClassInfo Word16 Word16 Word16 Word16 deriving (Show, Eq)

getInnerClass :: Get InnerClassInfo
getInnerClass = InnerClassInfo <$> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be

-- | Local variable info
data LocalVariable = LocalVariable Word16 Word16 Word16 Word16 Word16 deriving (Show, Eq)

getLocalVariable :: Get LocalVariable
getLocalVariable = LocalVariable <$> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be

-- | Local variable type info
data LocalVariableType = LocalVariableType Word16 Word16 Word16 Word16 Word16 deriving (Show, Eq)

getLocalVariableType :: Get LocalVariableType
getLocalVariableType = LocalVariableType <$> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be <*> getWord16be

-- | Annotation info
data Annotation = Annotation Word16 Word16 [(Word16, ElementValue)] deriving (Show, Eq)

getAnnotation :: Get Annotation
getAnnotation = do
  typeIndex <- getWord16be
  numPairs  <- getWord16be
  pairs     <- replicateN numPairs (liftA2 (,) getWord16be getElementValue)
  return $ Annotation typeIndex numPairs pairs

getParameterAnnotation :: Get (Word16, [Annotation])
getParameterAnnotation = do
  annotationCount <- getWord16be
  annotations     <- replicateN annotationCount getAnnotation
  return (annotationCount, annotations)

--data PrimitiveType =

-- | ADT for element values in attributes, potentially recursive
data ElementValue = BElementValue Word8 Word16
                  | CElementValue Word8 Word16
                  | DElementValue Word8 Word16
                  | FElementValue Word8 Word16
                  | IElementValue Word8 Word16
                  | JElementValue Word8 Word16
                  | ZElementValue Word8 Word16
                  | SElementValue Word8 Word16
                  | PrimitiveElementValue
                  | StringElementValue Word8 Word16
                  | EnumElementValue Word8 Word16 Word16
                  | ClassElementValue Word8 Word16
                  | AnnotationElementValue Word8 Annotation
                  | ArrayElementValue Word8 Word16 [ElementValue]
                  deriving (Show, Eq)

-- | Discriminates elements based on leading byte
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.7.16.1
getElementValue :: Get ElementValue
getElementValue = do
  tag <- getWord8
  case chr . fromEnum $ tag of
    'B' -> BElementValue tag <$> getWord16be
    'C' -> CElementValue tag <$> getWord16be
    'D' -> DElementValue tag <$> getWord16be
    'F' -> FElementValue tag <$> getWord16be
    'I' -> IElementValue tag <$> getWord16be
    'J' -> JElementValue tag <$> getWord16be
    'Z' -> ZElementValue tag <$> getWord16be
    'S' -> SElementValue tag <$> getWord16be
    's' -> StringElementValue tag <$> getWord16be
    'e' -> EnumElementValue tag <$> getWord16be <*> getWord16be
    'c' -> ClassElementValue tag <$> getWord16be
    '@' -> AnnotationElementValue tag <$> getAnnotation
    '[' -> do
      numValues <- getWord16be
      elements  <- replicateN numValues getElementValue
      return $ ArrayElementValue tag numValues elements
    _   -> error "Did not match any tag for element value"

data TypePath = TypePath Word8 [(Word8, Word8)]
  deriving (Show, Eq)

getTypePath :: Get TypePath
getTypePath = do
  pathLength <- getWord8
  path       <- replicateN pathLength ((,) <$> getWord8 <*> getWord8)
  return $ TypePath pathLength path

data TargetInfo = TypeParameterTarget Word8
                | SupertypeTarget Word16
                | TypeParameterBoundTarget Word8 Word8
                | EmptyTarget
                | FormalParameterTarget Word8
                | ThrowsTarget Word16
                | LocalvarTarget Word16 [(Word16, Word16, Word16)]
                | CatchTarget Word16
                | OffsetTarget Word16
                | TypeArgumentTarget Word16 Word8
                deriving (Show, Eq)

getLocalvarTarget :: Get TargetInfo
getLocalvarTarget = do
  tableLength <- getWord16be
  table       <- replicateN tableLength (liftA3 (,,) getWord16be getWord16be getWord16be)
  return $ LocalvarTarget tableLength table

getTargetInfo :: Word8 -> Get TargetInfo
getTargetInfo targetType
  | targetType <= 0x01 = TypeParameterTarget <$> getWord8
  | targetType == 0x10 = SupertypeTarget <$> getWord16be
  | targetType <= 0x12 = TypeParameterBoundTarget <$> getWord8 <*> getWord8
  | targetType <= 0x15 = return EmptyTarget
  | targetType == 0x16 = FormalParameterTarget <$> getWord8
  | targetType == 0x17 = ThrowsTarget <$> getWord16be
  | targetType <= 0x41 = getLocalvarTarget
  | targetType == 0x42 = CatchTarget <$> getWord16be
  | targetType <= 0x46 = OffsetTarget <$> getWord16be
  | targetType <= 0x4B = TypeArgumentTarget <$> getWord16be <*> getWord8

data TypeAnnotation = TypeAnnotation Word8 TargetInfo TypePath Word16 Word16 [(Word16, ElementValue)]
  deriving (Show, Eq)

getTypeAnnotation :: Get TypeAnnotation
getTypeAnnotation = do
  targetType <- getWord8
  targetInfo <- getTargetInfo targetType
  typePath   <- getTypePath
  typeIndex  <- getWord16be
  pairsCount <- getWord16be
  pairs      <- replicateN pairsCount ((,) <$> getWord16be <*> getElementValue)
  return $ TypeAnnotation targetType targetInfo typePath typeIndex pairsCount pairs


