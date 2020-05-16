module Internal.Attributes where

import           Control.Monad                      (guard, replicateM)
import           Data.Binary                        (Word16, Word32, Word8)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString.Lazy               as BL
import           Data.Char                          (chr)
import           Internal.Attributes.AttributeTypes
import           Internal.Attributes.StackMapFrame
import           Internal.ConstantPool
import Control.Applicative (liftA2, liftA3)

data Attribute
 = ConstantValue Word16
 | Code Word16 Word16 Word32 BL.ByteString Word16 [ExceptionHandler] Word16 [AttributeInfo]
 | StackMapTable Word16 [StackMapFrame]
 | Exceptions Word16 [Word16]
 | InnerClasses Word16 [InnerClassInfo]
 | EnclosingMethod Word16 Word16
 | Synthetic
 | Signature Word16
 | SourceFile Word16
 | SourceDebugExtension BL.ByteString
 | LineNumberTable Word16 [(Word16, Word16)]
 | LocalVariableTable Word16 [LocalVariable]
 | LocalVariableTypeTable Word16 [LocalVariableType]
 | Deprecated
 | RuntimeVisibleAnnotations Word16 [Annotation]
 | RuntimeInvisibleAnnotations Word16 [Annotation]
 | RuntimeVisibleParameterAnnotations Word8 [(Word16, [Annotation])]
 | RuntimeInvisibleParameterAnnotations Word8 [(Word16, [Annotation])]
 | RuntimeVisibleTypeAnnotations Word16 [TypeAnnotation]
 | RuntimeInvisibleTypeAnnotations Word16 [TypeAnnotation]
 | AnnotationDefault ElementValue
 | BootstrapMethods Word16 [(Word16, Word16, Word16)]
 | MethodParameters Word8 [(Word16, Word16)]
 | Unhandled BL.ByteString
 deriving (Show, Eq)

data AttributeInfo = AttributeInfo Word16 Word32 Attribute
  deriving (Show, Eq)

getAttributeInfo :: ConstantPool -> Get AttributeInfo
getAttributeInfo pool = do
  nameIndex          <- getWord16be
  attributeLength    <- getWord32be
  attribute          <- getAttribute pool nameIndex attributeLength
  return $ AttributeInfo nameIndex attributeLength attribute

---- | Parser for all attributes
---- | https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7
getAttribute :: ConstantPool -> Word16 -> Word32 -> Get Attribute
getAttribute pool nameIndex attributeLength =
  case poolConstantUtf8 pool nameIndex of
    "ConstantValue" ->
      ConstantValue <$> getWord16be
    "Code" -> do
      maxStack             <- getWord16be
      maxLocals            <- getWord16be
      codeLength           <- getWord32be
      code                 <- getLazyByteString (fromIntegral codeLength)
      exceptionTableLength <- getWord16be
      exceptionTable       <- replicateN exceptionTableLength getExceptionHandler
      attributesCount      <- getWord16be
      attributeInfo        <- replicateN attributesCount (getAttributeInfo pool)
      return $
        Code
          maxStack
          maxLocals
          codeLength
          code
          exceptionTableLength
          exceptionTable
          attributesCount
          attributeInfo
    "StackMapTable" -> do
      numberOfEntries <- getWord16be
      frameEntries    <- replicateN numberOfEntries getStackMapFrame
      return $ StackMapTable numberOfEntries frameEntries
    "Exceptions" -> do
      numberOfExceptions <- getWord16be
      exceptionIndices   <- replicateN numberOfExceptions getWord16be
      return $ Exceptions numberOfExceptions exceptionIndices
    "InnerClasses" -> do
      numberOfClasses <- getWord16be
      classes         <- replicateN numberOfClasses getInnerClass
      return $ InnerClasses numberOfClasses classes
    "EnclosingMethod" ->
      EnclosingMethod <$> getWord16be <*> getWord16be
    "Synthetic" ->
      return Synthetic
    "Signature" ->
      Signature <$> getWord16be
    "SourceFile" ->
      SourceFile <$> getWord16be
    "SourceDebugExtension" ->
      SourceDebugExtension <$> getLazyByteString (fromIntegral attributeLength)
    "LineNumberTable" -> do
      lineNumberTableLength <- getWord16be
      let getTable = replicateN lineNumberTableLength (liftA2 (,) getWord16be getWord16be)
      LineNumberTable lineNumberTableLength <$> getTable
    "LocalVariableTable" -> do
      tableLength <- getWord16be
      LocalVariableTable tableLength <$> replicateN tableLength getLocalVariable
    "LocalVariableTypeTable" -> do
      tableLength <- getWord16be
      LocalVariableTypeTable tableLength <$> replicateN tableLength getLocalVariableType
    "Deprecated" ->
      return  Deprecated
    "RuntimeVisibleAnnotations" -> do
      annotationCount <- getWord16be
      annotations     <- replicateN annotationCount getAnnotation
      return $ RuntimeVisibleAnnotations annotationCount annotations
    "RuntimeInvisibleAnnotations" -> do
      annotationCount <- getWord16be
      annotations     <- replicateN annotationCount getAnnotation
      return $ RuntimeInvisibleAnnotations annotationCount annotations
    "RuntimeVisibleParameterAnnotations" -> do
      parameters           <- getWord8
      parameterAnnotations <- replicateN parameters getParameterAnnotation
      return $ RuntimeVisibleParameterAnnotations parameters parameterAnnotations
    "RuntimeInvisibleParameterAnnotations" -> do
      parameters           <- getWord8
      parameterAnnotations <- replicateN parameters getParameterAnnotation
      return $ RuntimeInvisibleParameterAnnotations parameters parameterAnnotations
    "RuntimeVisibleTypeAnnotations" -> do
      annotationCount <- getWord16be
      annotations     <- replicateN annotationCount getTypeAnnotation
      return $ RuntimeVisibleTypeAnnotations annotationCount annotations
    "RuntimeInvisibleTypeAnnotations" -> do
      annotationCount <- getWord16be
      annotations     <- replicateN annotationCount getTypeAnnotation
      return $ RuntimeInvisibleTypeAnnotations annotationCount annotations
    "AnnotationDefault" ->
      AnnotationDefault <$> getElementValue
    "BootstrapMethods" -> do
      methodCount <- getWord16be
      methods     <- replicateN methodCount (liftA3 (,,) getWord16be getWord16be getWord16be)
      return $ BootstrapMethods methodCount methods
    "MethodParameters" -> do
      parameterCount <- getWord8
      parameters     <- replicateN parameterCount ((,) <$> getWord16be <*> getWord16be)
      return $ MethodParameters parameterCount parameters
    _ -> Unhandled <$> getLazyByteString (fromIntegral attributeLength)
