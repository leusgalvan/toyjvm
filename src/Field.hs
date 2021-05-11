module Field where

import ClassFile

data Field = Field {
    name :: String,
    className :: String
}

fromFieldInfo :: ConstantPool -> FieldInfo -> Field
fromFieldInfo = undefined 