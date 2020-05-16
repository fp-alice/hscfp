module Disassemble where

---- | For extracting values from a Word16 with .&.
--bitmaskExtractor :: Word16 -> [(Word16, a)] -> [a]
--bitmaskExtractor word matchers = snd <$> filter (\(mask, _) -> (word .&. mask) == mask) matchers
--
---- | Extracts a list of class access flags from a Word16
--getClassAccessFlags :: Word16 -> [ClassAccessFlag]
--getClassAccessFlags word = bitmaskExtractor word matchers
--  where
--    matchers = [(0x0001, ClassPublicAccess),
--                (0x0010, ClassFinalAccess),
--                (0x0020, ClassSuperAccess),
--                (0x0200, ClassInterfaceAccess),
--                (0x0400, ClassAbstractAccess),
--                (0x1000, ClassSyntheticAccess),
--                (0x2000, ClassAnnotationAccess),
--                (0x4000, ClassEnumAccess),
--                (0x8000, ClassModuleAccess)]
--
--
---- | Field access flags
---- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.5
--data FieldAccessFlag = FieldPublicAccess
--                     | FieldPrivateAccess
--                     | FieldProtectedAccess
--                     | FieldStaticAccess
--                     | FieldFinalAccess
--                     | FieldVolatileAccess
--                     | FieldTransientAccess
--                     | FieldSyntheticAccess
--                     | FieldEnumAccess
--
---- | Field info struct
----data FieldInfo = [FieldAccessFlag]
--
---- Extracts a list of field access flags from a Word16
--getFieldAccessFlags :: Word16 -> [FieldAccessFlag]
--getFieldAccessFlags word = bitmaskExtractor word matchers
--  where
--    matchers = [(0x0001, FieldPublicAccess),
--                (0x0002, FieldPrivateAccess),
--                (0x0004, FieldProtectedAccess),
--                (0x0008, FieldStaticAccess),
--                (0x0010, FieldFinalAccess),
--                (0x0040, FieldVolatileAccess),
--                (0x0080, FieldTransientAccess),
--                (0x1000, FieldSyntheticAccess),
--                (0x4000, FieldEnumAccess)]


-- | Class access flags
-- | https://docs.oracle.com/javase/specs/jvms/se11/html/jvms-4.html#jvms-4.1-200-E.1
--data ClassAccessFlag = ClassPublicAccess
--                     | ClassFinalAccess
--                     | ClassSuperAccess
--                     | ClassInterfaceAccess
--                     | ClassAbstractAccess
--                     | ClassSyntheticAccess
--                     | ClassAnnotationAccess
--                     | ClassEnumAccess
--                     | ClassModuleAccess
--                     deriving (Show, Eq)
