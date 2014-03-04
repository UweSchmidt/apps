module PPL.BuiltinFunctions
    ( buildinFcts ) where

import PPL.AbstractSyntax

buildinFcts     :: [(String, Type)]
buildinFcts
    = [ ("load",                picStr)
      , ("store",               voidPicStr)

      , ("width",               intPic)
      , ("height",              intPic)
      , ("black",               picInt2)
      , ("white",               picInt2)
      , ("grey",                picFloatInt2)

      , ("gamma",               picPicFloat)
      , ("invert",              picPic)
      , ("bitmap",              picPic)
      , ("blackAndWhite",       picPic)
      , ("reduceColor",         picPicInt)
      , ("flipVertical",        picPic)
      , ("flipHorizontal",      picPic)
      , ("flipDiagonal",        picPic)
      , ("rotate",              picPic)
      , ("shift",               picPicInt2)

      , ("cut",                 picPicInt4)
      , ("paste",               picPic2Int2)
      , ("scale",               picPicInt2)
      , ("shrink",              picPicInt2)
      , ("replicate",           picPicInt2)
      , ("resize",              picPicInt2)
      , ("sideBySide",          picPic2)
      , ("above",               picPic2)
      , ("partitionHorizontal", listPicPicInt)
      , ("partitionVertical",   listPicPicInt)
      , ("splitHorizontal",     listPicPicInt)
      , ("splitVertical",       listPicPicInt)
      , ("mergeHorizontal",     picPic2)
      , ("mergeVertical",       picPic2)
      , ("concatHorizontal",    picListPic)
      , ("concatVertical",      picListPic)

      , ("mean",                picPic2)
      , ("diff",                picPic2)
      , ("inverseMean",         picPic2)
      , ("inverseDiff",         picPic2)

      , ("exit",                voidVoid)
      , ("dump",                voidVoid)
      , ("abort",               voidStr)
      , ("write",               voidStr)
      , ("writeln",             voidStr)
      , ("getArgs",             FctType (ListType StringType) [])


      ]
      where
      voidVoid          = FctType VoidType      []
      voidStr           = FctType VoidType      [StringType]
      voidPicStr        = FctType VoidType      [PictureType, StringType]

      intPic            = FctType IntType       [PictureType]

      picPic            = FctType PictureType   [PictureType]
      picStr            = FctType PictureType   [StringType]
      picListPic        = FctType PictureType   [ListType PictureType]

      picInt2           = FctType PictureType   [IntType,       IntType]
      picPic2           = FctType PictureType   [PictureType,   PictureType]
      picPicFloat       = FctType PictureType   [PictureType,   FloatType]
      picPicInt         = FctType PictureType   [PictureType,   IntType]
      listPicPicInt     = FctType (ListType PictureType)        [PictureType,   IntType]

      picFloatInt2      = FctType PictureType   [FloatType,     IntType,IntType]
      picPicInt2        = FctType PictureType   [PictureType,   IntType,IntType]
      picPic2Int2       = FctType PictureType   [PictureType,   PictureType,    IntType,IntType]
      picPicInt4        = FctType PictureType   [PictureType,   IntType,IntType,IntType,IntType]

